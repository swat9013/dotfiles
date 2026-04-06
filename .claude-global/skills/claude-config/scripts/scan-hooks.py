#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""scan-hooks.py - settings.json と hooks スクリプトを検証して JSON で出力する"""

import json
import os
from pathlib import Path


def scan_settings_file(fpath: Path, label: str) -> dict:
    if not fpath.exists():
        return {"label": label, "exists": False}

    try:
        data = json.loads(fpath.read_text(encoding="utf-8"))
    except json.JSONDecodeError as e:
        return {"label": label, "exists": True, "parse_error": str(e)}

    result: dict = {"label": label, "exists": True}

    # $schema の有無
    schema_val = data.get("$schema")
    if schema_val:
        result["schema"] = schema_val
    else:
        result["schema"] = None

    # permissions 構造
    permissions = data.get("permissions")
    if permissions is not None:
        perm_keys = sorted(permissions.keys())
        allow = permissions.get("allow", [])
        deny = permissions.get("deny", [])

        conflicts = 0
        if allow and deny:
            deny_set = set(deny)
            conflicts = sum(1 for a in allow if a in deny_set)

        result["permissions"] = {
            "keys": perm_keys,
            "conflicts": conflicts,
        }
    else:
        result["permissions"] = None

    # hooks の有無と種別
    hooks = data.get("hooks")
    if hooks is not None:
        result["hooks"] = {"present": True, "types": sorted(hooks.keys())}
    else:
        result["hooks"] = {"present": False}

    return result


def collect_hook_commands(fpath: Path) -> list[str]:
    """settings.json から hooks[*][*].hooks[*].command を収集する"""
    if not fpath.exists():
        return []
    try:
        data = json.loads(fpath.read_text(encoding="utf-8"))
    except json.JSONDecodeError:
        return []

    hooks = data.get("hooks")
    if not hooks:
        return []

    commands = []
    for event_entries in hooks.values():
        if not isinstance(event_entries, list):
            continue
        for entry in event_entries:
            if not isinstance(entry, dict):
                continue
            for hook in entry.get("hooks", []):
                if not isinstance(hook, dict):
                    continue
                cmd = hook.get("command")
                if cmd:
                    commands.append(cmd)
    return commands


def check_script(command: str) -> dict:
    """コマンドの先頭トークンをスクリプトパスとして存在・実行権限を確認する"""
    script_path_str = command.split()[0] if command.split() else ""
    if not script_path_str:
        return {}

    # チルダ展開
    script_path = Path(script_path_str).expanduser()

    exists = script_path.exists()
    executable = os.access(script_path, os.X_OK) if exists else False

    return {
        "path": str(script_path),
        "exists": exists,
        "executable": executable,
    }


def build_warnings(settings: list[dict], hooks_scripts: list[dict]) -> list[str]:
    warnings = []

    for s in settings:
        if not s.get("exists"):
            continue
        if "parse_error" in s:
            warnings.append(f"{s['label']}: parse error - {s['parse_error']}")
            continue
        if s.get("schema") is None:
            warnings.append(f"{s['label']}: $schema is not set")
        perm = s.get("permissions")
        if perm and perm.get("conflicts", 0) > 0:
            warnings.append(
                f"{s['label']}: permissions.allow と deny に {perm['conflicts']} 個の矛盾するパターンが存在する"
            )

    for hs in hooks_scripts:
        if not hs.get("exists"):
            warnings.append(f"script not found: {hs['path']}")
        elif not hs.get("executable"):
            warnings.append(f"script not executable: {hs['path']}")

    return warnings


def main() -> None:
    project_root = Path.cwd()
    claude_dir = project_root / ".claude"

    settings_targets = [
        (claude_dir / "settings.json", ".claude/settings.json"),
        (claude_dir / "settings.local.json", ".claude/settings.local.json"),
    ]

    settings = [scan_settings_file(fpath, label) for fpath, label in settings_targets]

    # hooks-scripts スキャン
    seen_paths: set[str] = set()
    hooks_scripts: list[dict] = []
    for fpath, _ in settings_targets:
        for cmd in collect_hook_commands(fpath):
            info = check_script(cmd)
            if not info:
                continue
            path_key = info["path"]
            if path_key in seen_paths:
                continue
            seen_paths.add(path_key)
            hooks_scripts.append(info)

    warnings = build_warnings(settings, hooks_scripts)

    output = {
        "settings": settings,
        "hooks_scripts": hooks_scripts,
        "warnings": warnings,
        "warn_count": len(warnings),
    }

    print(json.dumps(output, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
