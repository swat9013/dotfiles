#!/usr/bin/env python3
"""
retrospective スキル用データ収集スクリプト

6つのシェルスクリプトを統合:
  find-sessions.sh, extract-messages.sh, prepare-sessions.sh,
  retro-setup.sh, collect-existing-context.sh, read-caches.sh

Usage:
    ./collect.py                    # 当日セッション
    ./collect.py --since=7d         # 過去7日分
    ./collect.py --since=2026-03-01 # 日付指定
    ./collect.py --limit=10         # 件数制限
"""

import argparse
import json
import re
import subprocess
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path


SIZE_MIN = 2000


def _get_repo_root(cwd: Path) -> Path:
    """git rev-parse --show-toplevel でリポジトリルートを取得。非gitならcwd。"""
    try:
        result = subprocess.run(
            ["git", "rev-parse", "--show-toplevel"],
            cwd=cwd, capture_output=True, text=True, check=True, timeout=5,
        )
        return Path(result.stdout.strip())
    except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
        return cwd


def _list_session_dirs(repo_root: Path) -> list[Path]:
    """~/.claude/projects/ からリポジトリに関連するセッションディレクトリを全列挙。

    条件: name == encoded_root OR name.startswith(encoded_root + "-")
    git管理外（削除済みworktree）のセッションディレクトリも包含する。
    """
    projects_dir = Path.home() / ".claude" / "projects"
    if not projects_dir.is_dir():
        return []
    encoded_root = re.sub(r"[^a-zA-Z0-9]", "-", str(repo_root))
    return [
        d for d in projects_dir.iterdir()
        if d.is_dir() and (d.name == encoded_root or d.name.startswith(encoded_root + "-"))
    ]


def find_sessions(session_dir: Path, cutoff: datetime) -> list[dict]:
    """セッションディレクトリ配下のメインセッション JSONL を検索する。

    Returns:
        [{"path": Path, "uuid": str, "size": int, "mtime": datetime}, ...]
    """
    candidates = []
    for f in session_dir.glob("*.jsonl"):
        if f.name.startswith("agent-"):
            continue
        st = f.stat()
        mtime = datetime.fromtimestamp(st.st_mtime, tz=timezone.utc)
        if mtime < cutoff:
            continue
        size = st.st_size
        if size < SIZE_MIN:
            continue
        candidates.append({
            "path": f,
            "uuid": f.stem,
            "size": size,
            "mtime": mtime,
        })
    return candidates


def extract_messages(jsonl_path: Path, max_chars: int = 10000) -> str:
    """セッションJSONLからUSER+ASSISTANTメッセージを抽出する。

    - TOOL_RESULTは除外（ノイズ削減で情報密度2-3倍向上）
    - assistantのtool_useは【tool: Name】マーカーのみ記録
    """
    output_parts = []
    total_chars = 0

    with open(jsonl_path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                continue

            msg_type = entry.get("type", "")
            if msg_type not in ("user", "assistant"):
                continue

            # isMeta（システム注入）はスキップ
            if entry.get("isMeta"):
                continue

            message = entry.get("message", {})
            content = message.get("content", "")
            text = _extract_content(content, msg_type)

            if not text:
                continue

            part = f"## {msg_type.upper()}\n{text}\n"

            if total_chars + len(part) > max_chars:
                remaining = max_chars - total_chars
                if remaining > 100:
                    output_parts.append(part[:remaining])
                    output_parts.append("\n[...truncated]")
                break

            output_parts.append(part)
            total_chars += len(part)

    return "\n".join(output_parts)


def collect_existing_context() -> str:
    """CLAUDE.md見出しとrules/ファイル一覧を収集する。"""
    parts = []

    # CLAUDE.md セクション見出し
    parts.append("## CLAUDE.md セクション見出し")
    claude_md = Path.cwd() / "CLAUDE.md"
    if claude_md.exists():
        headings = [
            line for line in claude_md.read_text(encoding="utf-8").splitlines()
            if line.startswith("##")
        ]
        parts.append("\n".join(headings) if headings else "(なし)")
    else:
        parts.append("(CLAUDE.mdなし)")

    parts.append("")

    # rules/ ファイル一覧
    parts.append("## rules/ ファイル一覧")
    rules_dir = Path.cwd() / ".claude" / "rules"
    if rules_dir.is_dir():
        files = sorted(f.name for f in rules_dir.iterdir() if f.is_file())
        parts.append("\n".join(files) if files else "(なし)")
    else:
        parts.append("(rules/なし)")

    return "\n".join(parts)


def collect_review_files() -> list[str]:
    """~/.claude/tmp/review/ の .md ファイルパス一覧を返す。"""
    review_dir = Path.home() / ".claude" / "tmp" / "review"
    if not review_dir.is_dir():
        return []
    return sorted(str(f) for f in review_dir.iterdir() if f.suffix == ".md")


def _parse_since(since: str) -> datetime:
    """--since引数をdatetimeに変換する。"""
    if since.endswith("d"):
        try:
            days = int(since[:-1])
        except ValueError:
            print(f"Error: invalid --since value: {since!r}", file=sys.stderr)
            sys.exit(1)
        return datetime.now(tz=timezone.utc) - timedelta(days=days)

    # YYYY-MM-DD形式
    try:
        dt = datetime.strptime(since, "%Y-%m-%d")
        return dt.replace(tzinfo=timezone.utc)
    except ValueError:
        print(f"Error: invalid --since value: {since!r} (expected Nd or YYYY-MM-DD)", file=sys.stderr)
        sys.exit(1)


def _extract_content(content, msg_type: str) -> str:
    """message.contentからテキストを抽出する。"""
    if isinstance(content, str):
        return _sanitize(content.strip())

    if isinstance(content, list):
        parts = []
        for item in content:
            if not isinstance(item, dict):
                continue
            item_type = item.get("type", "")

            if item_type == "text":
                text = item.get("text", "").strip()
                # system-reminder等のシステムメッセージをスキップ
                if re.match(
                    r"^<(system-reminder|ide_opened_file|ide_selection|"
                    r"local-command-caveat|local-command-stdout|command-name|"
                    r"command-message|command-args)\b",
                    text,
                ):
                    continue
                if text:
                    parts.append(text)
            elif item_type == "tool_use" and msg_type == "assistant":
                # tool_useはマーカーのみ記録
                name = item.get("name", "unknown")
                parts.append(f"【tool: {name}】")

        return _sanitize(" ".join(parts).strip())

    return ""


def _sanitize(text: str) -> str:
    """サロゲート文字等のJSON非互換文字を安全な文字に置換する。"""
    return text.encode("utf-8", errors="replace").decode("utf-8")


OUTPUT_DIR = Path.home() / ".claude" / "tmp" / "retrospective"


def _build_output_path() -> str:
    """日時付き出力パスを生成し、ディレクトリを作成する。"""
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
    return str(OUTPUT_DIR / f"{timestamp}.json")


def main():
    parser = argparse.ArgumentParser(
        description="retrospective用セッションデータを収集しJSONで出力する"
    )
    parser.add_argument(
        "--since", default="0d",
        help="期間指定: Nd（N日前）またはYYYY-MM-DD（デフォルト: 当日）"
    )
    parser.add_argument(
        "--limit", type=int, default=0,
        help="セッション件数上限（デフォルト: 0=無制限）"
    )
    args = parser.parse_args()

    # 1. 全セッションディレクトリ走査（現在リポジトリ関連のみ）
    cutoff = _parse_since(args.since)
    cwd = Path.cwd()
    review_files = collect_review_files()

    seen_dirs: set[Path] = set()
    all_candidates: list[dict] = []
    repo_root = _get_repo_root(cwd)
    for sd in _list_session_dirs(repo_root):
        if sd in seen_dirs:
            continue
        seen_dirs.add(sd)
        for c in find_sessions(sd, cutoff):
            c["session_dir_name"] = sd.name
            all_candidates.append(c)

    # 全 worktree 横断で新しい順にソート → limit 適用
    all_candidates.sort(key=lambda x: x["mtime"], reverse=True)
    if args.limit > 0:
        all_candidates = all_candidates[: args.limit]

    if not all_candidates:
        _write_output([], collect_existing_context(), 0, 0, review_files)
        return

    # 2. メッセージ抽出（親セッションのみ）
    session_data = []
    for s in all_candidates:
        messages = extract_messages(s["path"], max_chars=10000)
        if not messages.strip():
            continue

        session_data.append({
            "uuid": s["uuid"],
            "session_dir_name": s["session_dir_name"],
            "size": s["size"],
            "messages": messages,
        })

    # 3. 既存コンテキスト収集
    existing_context = collect_existing_context()

    # 4. ファイル出力 + サマリー表示
    skipped = len(all_candidates) - len(session_data)
    _write_output(session_data, existing_context, len(session_data), skipped, review_files)


def _write_output(
    sessions: list[dict], existing_context: str, total: int, skipped: int,
    review_files: list[str],
) -> None:
    """JSONをファイルに書き出し、サマリーをstdoutに表示する。"""
    output_file = _build_output_path()
    output = {
        "sessions": sessions,
        "existing_context": existing_context,
        "review_files": review_files,
        "stats": {"total": total, "skipped": skipped},
    }

    try:
        with open(output_file, "w", encoding="utf-8") as f:
            json.dump(output, f, ensure_ascii=False, indent=2)
    except OSError as e:
        print(f"Error: failed to write {output_file}: {e}", file=sys.stderr)
        sys.exit(1)

    # 親エージェント向けサマリー（stdoutに表示）
    session_dirs = {s.get("session_dir_name", "") for s in sessions if s.get("session_dir_name")}
    print(f"file={output_file}")
    print(f"total={total} skipped={skipped} session_dirs={len(session_dirs)}")
    if sessions:
        for s in sessions:
            wt_suffix = f" [{s['session_dir_name']}]" if s.get("session_dir_name") else ""
            print(f"  {s['uuid'][:8]}... {s['size']} bytes{wt_suffix}")
    else:
        print("対象セッションなし")


if __name__ == "__main__":
    main()
