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


def read_session_files(since_days: int) -> list[dict]:
    """グローバルセッションJSONLを走査してレコードリストを返す。

    agent- プレフィックスのファイルと mtime カットオフ対象を除外する。
    """
    import time
    projects_dir = Path.home() / ".claude" / "projects"
    if not projects_dir.exists():
        return []

    cutoff = time.time() - since_days * 86400
    records = []
    for jsonl_path in projects_dir.glob("*/*.jsonl"):
        # agent- プレフィックスのファイルを除外
        if jsonl_path.name.startswith("agent-"):
            continue
        # mtime カットオフ
        if jsonl_path.stat().st_mtime < cutoff:
            continue
        with open(jsonl_path, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    records.append(json.loads(line))
                except json.JSONDecodeError:
                    continue
    return records


def analyze_sessions(records: list[dict]) -> dict:
    """セッションレコードからメトリクスを集計する。"""
    if not records:
        return {
            "session_count": 0,
            "tool_usage": {},
            "skill_usage": {},
            "cache_hit_rate": None,
            "sidechain_rate": 0.0,
            "period": None,
        }

    tool_usage: dict[str, int] = defaultdict(int)
    skill_usage: dict[str, int] = defaultdict(int)
    total_cache_read = 0
    total_cache_creation = 0
    total_input = 0
    total_user = 0
    sidechain_user = 0
    session_ids: set[str] = set()

    for record in records:
        role = record.get("role", "")
        session_id = record.get("sessionId", record.get("session_id", ""))
        if session_id:
            session_ids.add(session_id)

        # tool_usage: assistant の tool_use を集計
        if role == "assistant":
            content = record.get("message", {}).get("content", [])
            if isinstance(content, list):
                for block in content:
                    if isinstance(block, dict) and block.get("type") == "tool_use":
                        tool_usage[block.get("name", "unknown")] += 1
                        # Skill ツール呼び出し時はスキル名を集計
                        if block.get("name") == "Skill":
                            input_data = block.get("input", {})
                            skill_name = input_data.get("skill", "unknown")
                            skill_usage[skill_name] += 1
            # キャッシュトークン集計
            usage = record.get("message", {}).get("usage", {})
            total_cache_read += usage.get("cache_read_input_tokens", 0)
            total_cache_creation += usage.get("cache_creation_input_tokens", 0)
            total_input += usage.get("input_tokens", 0)

        # sidechain_rate: user メッセージの sidechain フラグ
        if role == "user":
            total_user += 1
            if record.get("isSidechain") or record.get("sidechain"):
                sidechain_user += 1

    total_tokens = total_cache_read + total_cache_creation + total_input
    cache_hit_rate = (total_cache_read / total_tokens) if total_tokens > 0 else None
    sidechain_rate = (sidechain_user / total_user) if total_user > 0 else 0.0

    return {
        "session_count": len(session_ids),
        "tool_usage": dict(sorted(tool_usage.items(), key=lambda x: -x[1])),
        "skill_usage": dict(sorted(skill_usage.items(), key=lambda x: -x[1])),
        "cache_hit_rate": round(cache_hit_rate, 4) if cache_hit_rate is not None else None,
        "sidechain_rate": round(sidechain_rate, 4),
        "period": None,  # TASK-003 で period 計算を追加
    }


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
        "--since",
        default="14d",
        help="集計期間（例: 14d, 7d）。デフォルト: 14d",
    )
    args = parser.parse_args()

    m = re.match(r"(\d+)", args.since)
    since_days = int(m.group(1)) if m else 14

    entries = read_metrics_file(args.path)
    if entries:
        settings = load_settings(args.settings)
        allow_rules = settings.get("permissions", {}).get("allow", [])
        permissions_result = analyze(entries, allow_rules)
    else:
        permissions_result = {
            "tool_counts": {},
            "total": 0,
            "session_count": 0,
            "patterns": {},
            "unmatched": {},
            "suggestions": [],
        }

    session_records = read_session_files(since_days)
    session_metrics = analyze_sessions(session_records)

    print(json.dumps({"permissions": permissions_result, "sessions": session_metrics}, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
