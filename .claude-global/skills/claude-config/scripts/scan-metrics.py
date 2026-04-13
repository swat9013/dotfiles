#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""scan-metrics.py - permission-requests.jsonl を集計して JSON で出力する"""

import argparse
import json
from pathlib import Path


def read_metrics_file(path: Path | None = None) -> dict:
    if path is None:
        path = Path.home() / ".claude" / "tmp" / "metrics" / "permission-requests.jsonl"
    if not path.exists():
        return {"tool_counts": {}, "total": 0}
    tool_counts: dict[str, int] = {}
    total = 0
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                continue
            tool = entry.get("tool")
            if tool is None:
                continue
            tool_counts[tool] = tool_counts.get(tool, 0) + 1
            total += 1
    return {"tool_counts": tool_counts, "total": total}


def main() -> None:
    parser = argparse.ArgumentParser(description="permission-requests.jsonl を集計する")
    parser.add_argument(
        "--path",
        type=Path,
        default=None,
        help="入力ファイルパス（デフォルト: ~/.claude/tmp/metrics/permission-requests.jsonl）",
    )
    args = parser.parse_args()

    result = read_metrics_file(args.path)
    print(json.dumps(result, ensure_ascii=False))


if __name__ == "__main__":
    main()
