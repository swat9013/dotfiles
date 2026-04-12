#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""scan-review.py - tmp/review/ の頻出 dimension パターンを抽出し JSON で出力する"""

import json
import re
from collections import Counter
from pathlib import Path

FREQ_THRESHOLD = 3


def scan_review_dir(review_dir: Path) -> dict:
    if not review_dir.exists():
        return {
            "files_scanned": 0,
            "issue_count": 0,
            "frequent_dimensions": [],
            "warnings": [],
            "warn_count": 0,
        }

    md_files = list(review_dir.glob("*.md"))
    files_scanned = len(md_files)
    dimension_counter: Counter[str] = Counter()
    issue_count = 0

    for fpath in md_files:
        text = fpath.read_text(encoding="utf-8")
        for line in text.splitlines():
            line = line.strip()
            if not line.startswith("|"):
                continue
            cells = re.split(r"\|", line)
            # cells[0] is empty (before first |), cells[-1] is empty (after last |)
            # cell indices: [0]="", [1]=col1, [2]=col2, [3]=col3, [4]=dimension, ...
            if len(cells) < 6:
                continue
            # skip header row
            if "issue_id" in cells[1]:
                continue
            # skip separator row
            if "---" in cells[1]:
                continue
            dimension = cells[4].strip()
            if not dimension:
                continue
            dimension_counter[dimension] += 1
            issue_count += 1

    all_dimensions = [
        {"dimension": dim, "count": count}
        for dim, count in dimension_counter.most_common()
    ]
    frequent_dimensions = [d for d in all_dimensions if d["count"] >= FREQ_THRESHOLD]

    warnings: list[str] = []

    return {
        "files_scanned": files_scanned,
        "issue_count": issue_count,
        "all_dimensions": all_dimensions,
        "frequent_dimensions": frequent_dimensions,
        "warnings": warnings,
        "warn_count": len(warnings),
    }


def main() -> None:
    review_dir = Path.cwd() / ".claude" / "tmp" / "review"
    result = scan_review_dir(review_dir)
    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
