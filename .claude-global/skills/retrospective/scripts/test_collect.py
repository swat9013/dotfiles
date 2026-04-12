"""
test_collect.py — read_metrics_file のテスト仕様

テスト対象: collect.read_metrics_file(path: Path) -> dict
戻り値形式: {"tool_counts": {"ToolName": N, ...}, "total": N}
入力形式: JSONL（1行1JSON）。各行に "tool" フィールドが含まれる想定
"""

import json
from pathlib import Path

import pytest

from collect import read_metrics_file


# --- ケース1: ファイル不在 → 空集計を返す（例外なし）

def test_file_not_found_returns_empty(tmp_path):
    missing = tmp_path / "nonexistent.jsonl"
    result = read_metrics_file(missing)
    assert result == {"tool_counts": {}, "total": 0}


# --- ケース2: 不正 JSON 行を含む → 不正行を skip して正常行のみ集計

def test_invalid_json_lines_are_skipped(tmp_path):
    f = tmp_path / "metrics.jsonl"
    lines = [
        '{"tool": "Bash"}',
        'NOT_VALID_JSON',
        '{"tool": "Read"}',
    ]
    f.write_text("\n".join(lines), encoding="utf-8")

    result = read_metrics_file(f)
    assert result["tool_counts"] == {"Bash": 1, "Read": 1}
    assert result["total"] == 2


# --- ケース3: 正常な複数行 → tool 別カウントが正しい

def test_normal_lines_counted_per_tool(tmp_path):
    f = tmp_path / "metrics.jsonl"
    lines = [
        '{"tool": "Bash"}',
        '{"tool": "Bash"}',
        '{"tool": "Bash"}',
        '{"tool": "Read"}',
    ]
    f.write_text("\n".join(lines), encoding="utf-8")

    result = read_metrics_file(f)
    assert result["tool_counts"] == {"Bash": 3, "Read": 1}
    assert result["total"] == 4


# --- ケース4: "tool" キーが存在しない行 → skip（エラーにならない）

def test_missing_tool_key_is_skipped(tmp_path):
    f = tmp_path / "metrics.jsonl"
    lines = [
        '{"tool": "Bash"}',
        '{"other_field": "value"}',
        '{"tool": "Edit"}',
    ]
    f.write_text("\n".join(lines), encoding="utf-8")

    result = read_metrics_file(f)
    assert result["tool_counts"] == {"Bash": 1, "Edit": 1}
    assert result["total"] == 2
