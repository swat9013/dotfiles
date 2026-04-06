#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///

"""
scan-claude-md.py
CLAUDE.md と rules/ を検証する（scan-claude-md.sh の Python 実装）
"""

import json
import re
import sys
from pathlib import Path


def extract_frontmatter_paths(file_path: Path) -> str:
    """frontmatter の paths: 行を抽出する（単純な行読み取り）"""
    try:
        content = file_path.read_text(encoding="utf-8")
    except OSError:
        return ""

    # frontmatter ブロック内の paths: を探す
    in_frontmatter = False
    for i, line in enumerate(content.splitlines()):
        if i == 0 and line.strip() == "---":
            in_frontmatter = True
            continue
        if in_frontmatter and line.strip() == "---":
            break
        if in_frontmatter:
            m = re.match(r"^paths:\s*(.*)", line)
            if m:
                return m.group(1).strip()
    return ""


def scan_claude_md(project_root: Path) -> dict:
    """セクション1: CLAUDE.md スキャン"""
    claude_md = project_root / "CLAUDE.md"
    result = {}
    warnings = []

    if claude_md.is_file():
        lines = claude_md.read_text(encoding="utf-8").splitlines()
        line_count = len(lines)
        sections = [line for line in lines if re.match(r"^## ", line)]
        result["exists"] = True
        result["lines"] = line_count
        result["sections"] = sections
        if line_count > 150:
            status = f"WARN (lines {line_count} > 150)"
            warnings.append(f"CLAUDE.md: {status}")
        else:
            status = "PASS"
        result["status"] = status
    else:
        result["exists"] = False
        status = "WARN (CLAUDE.md not found)"
        result["status"] = status
        warnings.append(f"CLAUDE.md: {status}")

    return result, warnings


def scan_rules(claude_dir: Path) -> tuple[list, list]:
    """セクション2: rules/ スキャン"""
    rules_dir = claude_dir / "rules"
    results = []
    warnings = []

    if not rules_dir.is_dir():
        return results, warnings

    for fpath in sorted(rules_dir.iterdir()):
        if not fpath.is_file():
            continue
        lines = fpath.read_text(encoding="utf-8").splitlines()
        flines = len(lines)
        paths_value = extract_frontmatter_paths(fpath)

        entry = {
            "file": fpath.name,
            "lines": flines,
            "paths": paths_value if paths_value else "none",
        }
        if flines > 200:
            status = f"WARN (lines {flines} > 200)"
            warnings.append(f"rules/{fpath.name}: {status}")
        else:
            status = "PASS"
        entry["status"] = status
        results.append(entry)

    return results, warnings


def scan_rules_paths_check(project_root: Path, claude_dir: Path) -> tuple[list, list]:
    """セクション3: rules-paths-check（glob マッチ確認）"""
    rules_dir = claude_dir / "rules"
    results = []
    warnings = []

    if not rules_dir.is_dir():
        return results, warnings

    for fpath in sorted(rules_dir.iterdir()):
        if not fpath.is_file():
            continue
        paths_value = extract_frontmatter_paths(fpath)
        if not paths_value:
            continue

        # カンマ区切りの glob パターンを分割
        raw_globs = [g.strip() for g in paths_value.split(",")]
        globs = []

        for pattern in raw_globs:
            if not pattern:
                continue

            # Path.glob で再帰マッチ
            # パターンに / が含まれる場合は project_root 起点でそのまま使用
            # 含まれない場合は ** で囲んで再帰検索（find -name 相当）
            if "/" in pattern:
                # **/ を含むパターンはそのまま渡す
                matches = list(project_root.glob(pattern))
            else:
                # ファイル名のみのパターン: 再帰検索（find -name 相当）
                matches = list(project_root.glob(f"**/{pattern}"))

            match_count = len(matches)
            if match_count == 0:
                glob_status = "WARN"
                warnings.append(
                    f"rules/{fpath.name}: glob '{pattern}' -> no matches"
                )
            else:
                glob_status = "PASS"

            globs.append({
                "pattern": pattern,
                "matches": match_count,
                "status": glob_status,
            })

        results.append({
            "rule": fpath.name,
            "paths": paths_value,
            "globs": globs,
        })

    return results, warnings


def main() -> None:
    project_root = Path.cwd()
    claude_dir = project_root / ".claude"

    all_warnings = []

    claude_md_result, w1 = scan_claude_md(project_root)
    all_warnings.extend(w1)

    rules_result, w2 = scan_rules(claude_dir)
    all_warnings.extend(w2)

    rules_paths_result, w3 = scan_rules_paths_check(project_root, claude_dir)
    all_warnings.extend(w3)

    output = {
        "claude_md": claude_md_result,
        "rules": rules_result,
        "rules_paths_check": rules_paths_result,
        "warn_count": len(all_warnings),
        "warnings": all_warnings,
    }

    print(json.dumps(output, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
