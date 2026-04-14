#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""scan-metrics.py - permission-requests.jsonl を分析して改善提案を生成する

出力JSON構造:
  tool_counts: ツール別の総出現回数
  total: 全レコード数
  patterns: ツール別の key_info パターン集計（頻度順）
  unmatched: 既存 allow ルールにマッチしないパターン（提案候補）
  suggestions: 具体的な allow ルール追加提案
"""

import argparse
import json
import re
from collections import defaultdict
from fnmatch import fnmatch
from pathlib import Path


def load_settings(settings_path: Path | None = None) -> dict:
    if settings_path is None:
        settings_path = Path.home() / ".claude" / "settings.json"
    if not settings_path.exists():
        return {}
    with open(settings_path, "r", encoding="utf-8") as f:
        return json.load(f)


def extract_bash_prefix(command: str) -> str:
    """Bashコマンドから先頭のコマンド名（+サブコマンド）を抽出する。

    パイプやチェーンは最初のコマンドのみ。
    git, docker, glab, gog 等は2語目まで含める。
    """
    # パイプ・チェーンの前で切る
    cmd = re.split(r"\s*[|;&]+\s*", command)[0].strip()
    parts = cmd.split()
    if not parts:
        return ""
    exe = Path(parts[0]).name  # フルパスからコマンド名のみ
    multi_word_cmds = {"git", "docker", "glab", "gog", "npm", "pnpm", "yarn", "cargo", "go", "wt"}
    if exe in multi_word_cmds and len(parts) >= 2:
        return f"{exe} {parts[1]}"
    return exe


def extract_write_edit_path_pattern(file_path: str) -> str:
    """ファイルパスから汎化パターンを生成する。

    /Users/user/.dotfiles/.claude-global/skills/foo/SKILL.md
    → ~/.dotfiles/.claude-global/skills/**
    """
    home = str(Path.home())
    p = file_path
    if p.startswith(home):
        p = "~" + p[len(home):]

    # .claude/ 配下は2階層目まで保持
    m = re.match(r"(~/?[^/]*/)?(\.claude/[^/]+/)", p)
    if m:
        return m.group(0) + "**"

    # ghq 配下はリポジトリルートまで保持
    m = re.match(r"(~/ghq/[^/]+/[^/]+/[^/]+/)", p)
    if m:
        return m.group(1) + "**"

    # dotfiles 配下は3階層目まで保持
    m = re.match(r"(~/.dotfiles/[^/]+/[^/]+/)", p)
    if m:
        return m.group(1) + "**"

    # その他はディレクトリまで
    parent = str(Path(p).parent)
    return parent + "/**"


def matches_allow_rule(tool: str, key_info: str, allow_rules: list[str]) -> bool:
    """PermissionRequest が既存 allow ルールでカバーされるか判定する。"""
    for rule in allow_rules:
        # Bash(prefix:*) 形式
        m = re.match(r"Bash\((.+?):\*\)", rule)
        if m and tool == "Bash":
            prefix = m.group(1)
            if key_info.startswith(prefix):
                return True
            continue

        # Write/Edit/Read(glob) 形式
        m = re.match(r"(Write|Edit|Read)\((.+)\)", rule)
        if m and tool == m.group(1):
            glob_pattern = m.group(2)
            # ~ を展開
            expanded = glob_pattern.replace("~", str(Path.home()))
            if fnmatch(key_info, expanded):
                return True
            # ** を含むパターンは簡易マッチ
            if "**" in glob_pattern:
                base = expanded.split("**")[0]
                if key_info.startswith(base):
                    return True
            continue

        # ツール名のみ（Skill, Task(Explore) 等）
        if rule == tool:
            return True

        # mcp ワイルドカード
        if rule.endswith("*") and not rule.startswith("Bash("):
            prefix = rule[:-1]
            if tool.startswith(prefix):
                return True

    return False


def generate_suggestion(tool: str, pattern: str, count: int) -> dict | None:
    """パターンから具体的な allow ルール提案を生成する。"""
    if tool == "Bash":
        # sed, chmod, xxd 等 → Bash(cmd:*)
        cmd = pattern.split()[0] if pattern else ""
        if cmd:
            return {
                "rule": f"Bash({pattern}:*)",
                "reason": f"{pattern} が {count} 回出現",
                "risk": assess_risk(tool, pattern),
            }
    elif tool in ("Write", "Edit"):
        return {
            "rule": f"{tool}({pattern})",
            "reason": f"{tool} to {pattern} が {count} 回出現",
            "risk": assess_risk(tool, pattern),
        }
    elif tool in ("Read", "Glob", "WebSearch", "WebFetch", "AskUserQuestion"):
        # これらは allow 追加で制御すべきか判断が必要
        return {
            "rule": tool,
            "reason": f"{tool} が {count} 回出現",
            "risk": "low",
            "note": "ツール全体の allow を検討",
        }
    return None


def assess_risk(tool: str, pattern: str) -> str:
    """リスクレベルを判定する。"""
    high_risk = {"rm", "rm -rf", "chmod", "kill", "pkill"}
    medium_risk = {"curl", "git merge", "git rebase", "git reset"}

    if tool == "Bash":
        cmd = pattern.split()[0] if pattern else ""
        if cmd in high_risk or pattern in high_risk:
            return "high"
        if cmd in medium_risk or pattern in medium_risk:
            return "medium"
        return "low"

    if tool in ("Write", "Edit"):
        if any(s in pattern for s in [".env", "secret", "credential", "key", ".ssh"]):
            return "high"
        if "settings.json" in pattern:
            return "medium"
        return "low"

    return "low"


def read_metrics_file(path: Path | None = None) -> list[dict]:
    if path is None:
        path = Path.home() / ".claude" / "tmp" / "metrics" / "permission-requests.jsonl"
    if not path.exists():
        return []
    entries = []
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entries.append(json.loads(line))
            except json.JSONDecodeError:
                continue
    return entries


def analyze(entries: list[dict], allow_rules: list[str]) -> dict:
    tool_counts: dict[str, int] = defaultdict(int)
    pattern_counts: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    session_tools: dict[str, set[str]] = defaultdict(set)
    unmatched: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))

    for entry in entries:
        tool = entry.get("tool", "unknown")
        key_info = entry.get("key_info", "")
        session_id = entry.get("session_id", "unknown")

        tool_counts[tool] += 1
        session_tools[session_id].add(tool)

        # パターン抽出
        if tool == "Bash":
            pattern = extract_bash_prefix(key_info)
        elif tool in ("Write", "Edit"):
            pattern = extract_write_edit_path_pattern(key_info)
        else:
            pattern = tool  # Glob, WebSearch 等はツール名自体がパターン

        pattern_counts[tool][pattern] += 1

        # allow ルールとの照合
        if not matches_allow_rule(tool, key_info, allow_rules):
            unmatched[tool][pattern] += 1

    # パターンを頻度順にソート
    sorted_patterns = {}
    for tool, patterns in pattern_counts.items():
        sorted_patterns[tool] = dict(sorted(patterns.items(), key=lambda x: -x[1]))

    sorted_unmatched = {}
    for tool, patterns in unmatched.items():
        sorted_unmatched[tool] = dict(sorted(patterns.items(), key=lambda x: -x[1]))

    # 提案生成（2回以上出現 + allow 未マッチのみ）
    suggestions = []
    for tool, patterns in sorted_unmatched.items():
        for pattern, count in patterns.items():
            if count >= 2:
                suggestion = generate_suggestion(tool, pattern, count)
                if suggestion:
                    suggestions.append(suggestion)

    # リスク低→高の順でソート（低リスクほど採用しやすい）
    risk_order = {"low": 0, "medium": 1, "high": 2}
    suggestions.sort(key=lambda s: (risk_order.get(s["risk"], 3), -int(re.search(r"\d+", s["reason"]).group())))

    return {
        "tool_counts": dict(tool_counts),
        "total": len(entries),
        "session_count": len(session_tools),
        "patterns": sorted_patterns,
        "unmatched": sorted_unmatched,
        "suggestions": suggestions,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="permission-requests.jsonl を分析する")
    parser.add_argument(
        "--path",
        type=Path,
        default=None,
        help="入力ファイルパス（デフォルト: ~/.claude/tmp/metrics/permission-requests.jsonl）",
    )
    parser.add_argument(
        "--settings",
        type=Path,
        default=None,
        help="settings.json パス（デフォルト: ~/.claude/settings.json）",
    )
    parser.add_argument(
        "--summary",
        action="store_true",
        help="サマリのみ出力（suggestions + tool_counts）",
    )
    args = parser.parse_args()

    entries = read_metrics_file(args.path)
    if not entries:
        print(json.dumps({"tool_counts": {}, "total": 0, "suggestions": []}, ensure_ascii=False))
        return

    settings = load_settings(args.settings)
    allow_rules = settings.get("permissions", {}).get("allow", [])

    result = analyze(entries, allow_rules)

    if args.summary:
        output = {
            "total": result["total"],
            "session_count": result["session_count"],
            "tool_counts": result["tool_counts"],
            "suggestions": result["suggestions"],
        }
    else:
        output = result

    print(json.dumps(output, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
