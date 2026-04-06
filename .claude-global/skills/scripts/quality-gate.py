#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""quality-gate.py - Lint/Test/Build コマンドの検出・実行・JSON結果報告

Usage: quality-gate.py [--lint-cmd=CMD] [--test-cmd=CMD] [--build-cmd=CMD] [project_dir]
"""

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Detect and run lint/test/build commands, report results as JSON."
    )
    parser.add_argument("--lint-cmd", default="", help="Override lint command")
    parser.add_argument("--test-cmd", default="", help="Override test command")
    parser.add_argument("--build-cmd", default="", help="Override build command")
    parser.add_argument("project_dir", nargs="?", default=".", help="Project directory (default: .)")
    return parser.parse_args()


def detect_project_type(project_dir: Path) -> str:
    if (project_dir / "package.json").exists():
        return "node"
    if (project_dir / "pyproject.toml").exists():
        return "python"
    if (project_dir / "Cargo.toml").exists():
        return "rust"
    if (project_dir / "go.mod").exists():
        return "go"
    if (project_dir / "Makefile").exists():
        return "make"
    return "unknown"


def detect_node_commands(project_dir: Path, lint: str, test: str, build: str) -> tuple[str, str, str]:
    try:
        pkg = json.loads((project_dir / "package.json").read_text())
        scripts = pkg.get("scripts", {})
    except Exception:
        scripts = {}

    if not lint and scripts.get("lint"):
        lint = "npm run lint"
    if not test and scripts.get("test"):
        test = "npm test"
    if not build and scripts.get("build"):
        build = "npm run build"
    return lint, test, build


def detect_python_commands(project_dir: Path, lint: str, test: str) -> tuple[str, str]:
    try:
        content = (project_dir / "pyproject.toml").read_text()
    except Exception:
        content = ""

    if not lint:
        if "[tool.ruff]" in content:
            lint = "uv run ruff check ."
        elif "[tool.flake8]" in content or (project_dir / ".flake8").exists():
            lint = "flake8 ."

    if not test:
        if "[tool.pytest" in content or (project_dir / "pytest.ini").exists():
            test = "pytest"

    return lint, test


def detect_rust_commands(lint: str, test: str, build: str) -> tuple[str, str, str]:
    if not lint:
        lint = "cargo fmt --check && cargo clippy -- -D warnings"
    if not test:
        test = "cargo test"
    if not build:
        build = "cargo build"
    return lint, test, build


def detect_go_commands(lint: str, test: str, build: str) -> tuple[str, str, str]:
    if not lint:
        lint = "go vet ./..."
    if not test:
        test = "go test ./..."
    if not build:
        build = "go build ./..."
    return lint, test, build


def detect_make_commands(project_dir: Path, lint: str, test: str, build: str) -> tuple[str, str, str]:
    try:
        content = (project_dir / "Makefile").read_text()
    except Exception:
        return lint, test, build

    if not lint and re.search(r"^lint\s*:", content, re.MULTILINE):
        lint = "make lint"
    if not test and re.search(r"^test\s*:", content, re.MULTILINE):
        test = "make test"
    if not build and re.search(r"^build\s*:", content, re.MULTILINE):
        build = "make build"
    return lint, test, build


def run_command(label: str, cmd: str, cwd: Path) -> dict:
    if not cmd:
        return {"label": label, "command": None, "status": "SKIP", "output": ""}

    # shell=True: パイプやリダイレクトを含むコマンド文字列を正しく実行するため。
    # 呼び出し元はスキル内部のみ（外部入力は想定しない）。既存 eval と同等のリスク。
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True, cwd=cwd)  # noqa: S602
    output = (result.stdout + result.stderr).strip()

    if result.returncode == 0:
        return {"label": label, "command": cmd, "status": "PASS", "output": output}
    else:
        # エラー出力は末尾30行に絞る（quality-gate.sh と同等）
        lines = output.splitlines()
        trimmed = "\n".join(lines[-30:]) if len(lines) > 30 else output
        return {"label": label, "command": cmd, "status": "FAIL", "output": trimmed}


def main() -> None:
    args = parse_args()
    project_dir = Path(args.project_dir).resolve()

    if not project_dir.is_dir():
        print(json.dumps({"error": f"Cannot access directory: {project_dir}"}), file=sys.stderr)
        sys.exit(1)

    lint_cmd: str = args.lint_cmd
    test_cmd: str = args.test_cmd
    build_cmd: str = args.build_cmd

    project_type = detect_project_type(project_dir)

    if project_type == "node":
        lint_cmd, test_cmd, build_cmd = detect_node_commands(project_dir, lint_cmd, test_cmd, build_cmd)
    elif project_type == "python":
        lint_cmd, test_cmd = detect_python_commands(project_dir, lint_cmd, test_cmd)
    elif project_type == "rust":
        lint_cmd, test_cmd, build_cmd = detect_rust_commands(lint_cmd, test_cmd, build_cmd)
    elif project_type == "go":
        lint_cmd, test_cmd, build_cmd = detect_go_commands(lint_cmd, test_cmd, build_cmd)
    elif project_type == "make":
        lint_cmd, test_cmd, build_cmd = detect_make_commands(project_dir, lint_cmd, test_cmd, build_cmd)

    # Makefile はフォールバックとして常にチェック（他の種別でも Makefile がある場合）
    if project_type != "make" and (project_dir / "Makefile").exists():
        lint_cmd, test_cmd, build_cmd = detect_make_commands(project_dir, lint_cmd, test_cmd, build_cmd)

    if not lint_cmd and not test_cmd and not build_cmd:
        output = {
            "project_type": project_type,
            "commands": {"lint": None, "test": None, "build": None},
            "results": [],
            "gate": "SKIP",
            "message": "No lint/test/build commands detected.",
        }
        print(json.dumps(output, ensure_ascii=False, indent=2))
        return

    results = [
        run_command("LINT", lint_cmd, project_dir),
        run_command("TEST", test_cmd, project_dir),
        run_command("BUILD", build_cmd, project_dir),
    ]

    failed = any(r["status"] == "FAIL" for r in results)
    gate = "FAIL" if failed else "PASS"

    output = {
        "project_type": project_type,
        "commands": {
            "lint": lint_cmd or None,
            "test": test_cmd or None,
            "build": build_cmd or None,
        },
        "results": results,
        "gate": gate,
    }
    print(json.dumps(output, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
