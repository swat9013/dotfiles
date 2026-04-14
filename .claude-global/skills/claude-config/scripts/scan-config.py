#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///

import argparse
import json
import os
import re
from pathlib import Path


def scan_claude_dir(claude_dir: Path) -> dict:
    if not claude_dir.is_dir():
        return {"name": ".claude/ directory structure", "entries": None, "error": f"directory not found: {claude_dir}"}

    entries = sorted(
        f"{p.name}/" if p.is_dir() else p.name
        for p in claude_dir.iterdir()
    )
    return {"name": ".claude/ directory structure", "entries": entries}


def scan_project_context(project_root: Path) -> dict:
    files = ["package.json", "pyproject.toml", "Cargo.toml", "go.mod", "Makefile"]
    paths = [".github/workflows", ".gitlab-ci.yml"]

    items = [
        {"name": name, "exists": (project_root / name).is_file()}
        for name in files
    ] + [
        {"name": name, "exists": (project_root / name).exists()}
        for name in paths
    ]
    return {"name": "project-context", "items": items}


def main() -> None:
    parser = argparse.ArgumentParser(description="Scan Claude Code configuration and output as JSON.")
    parser.add_argument("--project-root", default=os.environ.get("PWD", "."), help="Project root directory (default: $PWD)")
    args = parser.parse_args()

    project_root = Path(args.project_root)
    claude_dir = project_root / ".claude"

    # encoded_cwd: memory/ パスを特定するためのハイフン区切り文字列
    # collect.py と同一の re.sub ロジック。二重管理だが Computational First 優先
    encoded_cwd = re.sub(r"[^a-zA-Z0-9]", "-", str(project_root))

    result = {
        "encoded_cwd": encoded_cwd,
        "sections": [
            scan_claude_dir(claude_dir),
            scan_project_context(project_root),
        ]
    }

    print(json.dumps(result, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
