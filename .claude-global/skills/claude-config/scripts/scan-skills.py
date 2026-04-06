#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///

import json
import re
from pathlib import Path


SKILL_DIRS = [
    (Path.home() / ".dotfiles/.claude-global/skills", ".claude-global/skills"),
    (Path.home() / ".dotfiles/.claude/skills", ".claude/skills"),
]

LINES_LIMIT = 500
DESC_LIMIT = 130
REF_LINES_LIMIT = 200
CLI_WRAPPER_LINE_LIMIT = 30

BLOCK_SCALARS = {"|-", ">-", "|", ">"}

# --- description extraction ---

def extract_description(skill_md: Path) -> str:
    lines = skill_md.read_text(encoding="utf-8").splitlines()
    for i, line in enumerate(lines):
        if not line.startswith("description:"):
            continue
        val = line[len("description:"):].strip()
        if val not in BLOCK_SCALARS:
            return val
        # block scalar: collect subsequent indented lines
        parts = []
        for subsequent in lines[i + 1:]:
            if subsequent and subsequent[0] in (" ", "\t"):
                parts.append(subsequent.strip())
            else:
                break
        return " ".join(parts)
    return ""


# --- script language heuristics ---

def _has_multiline_awk(content: str) -> bool:
    """awk block with 3+ lines (awk '...' spanning 3+ lines)."""
    # Match awk '...{...}' blocks spanning multiple lines
    awk_blocks = re.findall(r"awk\s+'([^']*)'", content, re.DOTALL)
    for block in awk_blocks:
        inner_lines = [l for l in block.splitlines() if l.strip()]
        if len(inner_lines) >= 3:
            return True
    # Also check for awk with double quotes
    awk_blocks_dq = re.findall(r'awk\s+"([^"]*)"', content, re.DOTALL)
    for block in awk_blocks_dq:
        inner_lines = [l for l in block.splitlines() if l.strip()]
        if len(inner_lines) >= 3:
            return True
    return False


def _has_jq_deep_pipe(content: str) -> bool:
    """jq pipe with 3+ stages."""
    # Find jq filter expressions (single-quoted or double-quoted)
    jq_exprs = re.findall(r"jq\s+(?:-[a-zA-Z]+\s+)*'([^']*)'", content, re.DOTALL)
    jq_exprs += re.findall(r'jq\s+(?:-[a-zA-Z]+\s+)*"([^"]*)"', content, re.DOTALL)
    for expr in jq_exprs:
        # Count pipe characters not inside strings
        pipe_count = expr.count("|")
        if pipe_count >= 3:
            return True
    return False


def _has_while_read_parse(content: str) -> bool:
    """while read + line parsing."""
    return bool(re.search(r"while\b.*\bread\b", content))


def _has_long_function(content: str) -> bool:
    """Function longer than 50 lines."""
    lines = content.splitlines()
    in_func = False
    func_start = 0
    depth = 0

    for i, line in enumerate(lines):
        stripped = line.strip()
        if not in_func:
            # Match function declarations: name() { or function name() {
            if re.match(r"^(?:function\s+)?\w+\s*\(\s*\)\s*\{?\s*$", stripped):
                in_func = True
                func_start = i
                depth = 1 if stripped.endswith("{") else 0
                continue
        else:
            depth += stripped.count("{") - stripped.count("}")
            if depth <= 0:
                func_len = i - func_start + 1
                if func_len > 50:
                    return True
                in_func = False
                depth = 0
    return False


def _is_cli_wrapper(content: str, total_lines: int) -> bool:
    """30 lines or fewer AND no heuristic matches → treat as CLI wrapper (PASS)."""
    if total_lines > CLI_WRAPPER_LINE_LIMIT:
        return False
    return not (
        _has_multiline_awk(content)
        or _has_jq_deep_pipe(content)
        or _has_while_read_parse(content)
        or _has_long_function(content)
    )


def check_script_language(sh_file: Path) -> list[dict]:
    """Return list of triggered heuristics for a .sh file."""
    content = sh_file.read_text(encoding="utf-8")
    total_lines = len(content.splitlines())

    if _is_cli_wrapper(content, total_lines):
        return []

    hits = []
    if _has_multiline_awk(content):
        hits.append("awk_multiline")
    if _has_jq_deep_pipe(content):
        hits.append("jq_deep_pipe")
    if _has_while_read_parse(content):
        hits.append("while_read_parse")
    if _has_long_function(content):
        hits.append("long_function")
    return hits


# --- per-skill checks ---

def check_skill(skill_dir: Path) -> dict:
    name = skill_dir.name
    skill_md = skill_dir / "SKILL.md"

    if not skill_md.exists():
        return {"name": name, "skip": "no SKILL.md"}

    checks = []
    warnings = []

    # Check 1: line count
    line_count = len(skill_md.read_text(encoding="utf-8").splitlines())
    status = "PASS" if line_count <= LINES_LIMIT else "WARN"
    checks.append({"check": "lines", "value": line_count, "limit": LINES_LIMIT, "status": status})
    if status == "WARN":
        warnings.append(f"[{name}] SKILL.md lines: {line_count} > {LINES_LIMIT}")

    # Check 2: description length
    desc = extract_description(skill_md)
    desc_len = len(desc)
    status = "PASS" if desc_len <= DESC_LIMIT else "WARN"
    checks.append({"check": "description_length", "value": desc_len, "limit": DESC_LIMIT, "status": status})
    if status == "WARN":
        warnings.append(f"[{name}] description length: {desc_len} > {DESC_LIMIT}")

    # Check 3: Use when
    status = "PASS" if "Use when" in desc else "WARN"
    checks.append({"check": "use_when", "status": status})
    if status == "WARN":
        warnings.append(f"[{name}] description missing 'Use when'")

    # Check 4: .claude/commands/ directory
    commands_dir = skill_dir / ".claude" / "commands"
    status = "WARN" if commands_dir.is_dir() else "PASS"
    checks.append({"check": "commands_dir", "status": status})
    if status == "WARN":
        warnings.append(f"[{name}] .claude/commands/ found (未移行スキルの可能性)")

    # Check 5: references line count
    refs = []
    refs_dir = skill_dir / "references"
    if refs_dir.is_dir():
        for ref_file in sorted(refs_dir.glob("*.md")):
            ref_lines = len(ref_file.read_text(encoding="utf-8").splitlines())
            ref_status = "PASS" if ref_lines <= REF_LINES_LIMIT else "WARN"
            refs.append({"file": ref_file.name, "lines": ref_lines, "limit": REF_LINES_LIMIT, "status": ref_status})
            if ref_status == "WARN":
                warnings.append(f"[{name}] references/{ref_file.name}: {ref_lines} lines > {REF_LINES_LIMIT}")

    # Check 6: third-person (first word must not be I or You)
    first_word = desc.split()[0] if desc.split() else ""
    status = "WARN" if first_word in ("I", "You") else "PASS"
    checks.append({"check": "third_person", "status": status})
    if status == "WARN":
        warnings.append(f"[{name}] description starts with '{first_word}' (not third-person)")

    # Script language diagnostic
    script_language = []
    scripts_dir = skill_dir / "scripts"
    if scripts_dir.is_dir():
        for sh_file in sorted(scripts_dir.glob("*.sh")):
            hits = check_script_language(sh_file)
            if hits:
                entry = {"file": sh_file.name, "status": "WARN", "reasons": hits}
                script_language.append(entry)
                warnings.append(f"[{name}] scripts/{sh_file.name}: script language WARN ({', '.join(hits)})")

    return {
        "name": name,
        "checks": checks,
        "references": refs,
        "script_language": script_language,
        "_warnings": warnings,
    }


# --- scan a directory ---

def scan_dir(base_dir: Path, label: str) -> dict:
    if not base_dir.is_dir():
        return {"label": label, "skills": [], "error": f"directory not found: {base_dir}"}

    skills = []
    all_warnings = []

    exclude = {"_shared", "scripts"}
    for skill_dir in sorted(base_dir.iterdir()):
        if not skill_dir.is_dir():
            continue
        if skill_dir.name in exclude:
            continue
        result = check_skill(skill_dir)
        all_warnings.extend(result.pop("_warnings", []))
        skills.append(result)

    return {"label": label, "skills": skills, "_warnings": all_warnings}


def main() -> None:
    scans = []
    all_warnings = []

    for base_dir, label in SKILL_DIRS:
        scan = scan_dir(base_dir, label)
        all_warnings.extend(scan.pop("_warnings", []))
        scans.append(scan)

    output = {
        "scans": scans,
        "warn_count": len(all_warnings),
        "warnings": all_warnings,
    }
    print(json.dumps(output, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
