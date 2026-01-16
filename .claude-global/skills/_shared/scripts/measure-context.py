#!/usr/bin/env python3
"""
measure-context.py - コンテキスト規模計測スクリプト
コンテキスト規模計測（domain-context, config-optimizer で共有）
macOS / Linux 両対応
"""

import argparse
import json
from pathlib import Path

# 計測対象の設定
TARGETS = {
    "claude_md": {
        "path": "CLAUDE.md",
        "type": "file",
        "threshold": {"recommended": 1500, "limit": 2000},
    },
    "docs": {
        "path": "docs",
        "type": "dir",
        "recursive": True,
        "threshold": {"recommended": 3000, "limit": 5000},
    },
}


def estimate_tokens(text: str) -> int:
    """トークン数を概算（ASCII: 4文字≈1トークン、非ASCII: 1文字≈2トークン）"""
    ascii_chars = sum(1 for c in text if ord(c) < 128)
    non_ascii_chars = len(text) - ascii_chars
    return int(ascii_chars / 4 + non_ascii_chars * 2)


def get_status(tokens: int, recommended: int, limit: int) -> str:
    """閾値に基づいてステータスを判定"""
    if tokens <= recommended:
        return "ok"
    elif tokens <= limit:
        return "warning"
    return "error"


def measure_file(file_path: Path, threshold: dict) -> dict:
    """単一ファイルを計測"""
    try:
        text = file_path.read_text(encoding="utf-8", errors="ignore")
        lines = len(text.splitlines())
        tokens = estimate_tokens(text)
        status = get_status(tokens, threshold["recommended"], threshold["limit"])
        return {
            "threshold": threshold,
            "file": {
                "name": file_path.name,
                "lines": lines,
                "tokens": tokens,
                "status": status,
            },
        }
    except FileNotFoundError:
        return {"error": "file not found"}


def measure_directory(dir_path: Path, threshold: dict, recursive: bool = False) -> dict:
    """ディレクトリ内の.mdファイルを計測"""
    files_data = []
    total_lines = 0
    total_tokens = 0
    over_limit = 0

    if dir_path.exists():
        pattern = "**/*.md" if recursive else "*.md"
        md_files = sorted(dir_path.glob(pattern))

        for file_path in md_files:
            try:
                text = file_path.read_text(encoding="utf-8", errors="ignore")
                lines = len(text.splitlines())
                tokens = estimate_tokens(text)
                status = get_status(tokens, threshold["recommended"], threshold["limit"])

                name = str(file_path.relative_to(dir_path)) if recursive else file_path.name
                files_data.append({
                    "name": name,
                    "lines": lines,
                    "tokens": tokens,
                    "status": status,
                })

                total_lines += lines
                total_tokens += tokens
                if status == "error":
                    over_limit += 1
            except Exception:
                continue

    return {
        "threshold": threshold,
        "files": files_data,
        "summary": {
            "file_count": len(files_data),
            "total_lines": total_lines,
            "total_tokens": total_tokens,
            "over_limit": over_limit,
        },
    }


def main():
    parser = argparse.ArgumentParser(
        description="コンテキスト規模計測スクリプト（トークン数ベース）"
    )
    for name in TARGETS:
        parser.add_argument(f"--{name}", action="store_true", help=f"{name} のみ計測")
    args = parser.parse_args()

    # 指定されたターゲット（なければ全て）
    selected = [name for name in TARGETS if getattr(args, name)]
    if not selected:
        selected = list(TARGETS.keys())

    project_root = Path.cwd()
    result = {}

    for name in selected:
        config = TARGETS[name]
        target_path = project_root / config["path"]

        if not target_path.exists():
            continue

        if config["type"] == "file":
            result[name] = measure_file(target_path, config["threshold"])
        else:
            result[name] = measure_directory(
                target_path, config["threshold"], config.get("recursive", False)
            )

    print(json.dumps(result, indent=2, ensure_ascii=False))


if __name__ == "__main__":
    main()
