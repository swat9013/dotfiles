"""test_scan_review.py - scan_review_dir の仕様テスト（テストファースト）"""

from pathlib import Path

import pytest

from scan_review import scan_review_dir

FREQ_THRESHOLD = 3


def _make_review_md(path: Path, rows: list[dict]) -> None:
    """Markdownテーブルファイルを生成するヘルパー"""
    header = "| issue_id | file | line | dimension | problem | suggestion |"
    sep = "|----------|------|------|-----------|---------|------------|"
    lines = [header, sep]
    for i, row in enumerate(rows, start=1):
        lines.append(
            f"| REVIEW-1-{i} | {row.get('file', 'x.py')} | {row.get('line', 1)} "
            f"| {row['dimension']} | 問題{i} | 修正案{i} |"
        )
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


# ── テストケース 1: ディレクトリ不在 ────────────────────────────────────────

def test_returns_zero_result_when_dir_not_exist(tmp_path: Path) -> None:
    """ディレクトリが存在しない場合は空結果を返し、例外を起こさない"""
    missing = tmp_path / "nonexistent"
    result = scan_review_dir(missing)

    assert result == {
        "files_scanned": 0,
        "issue_count": 0,
        "frequent_dimensions": [],
        "warnings": [],
        "warn_count": 0,
    }


# ── テストケース 2: dimension フィールドを集計できる ────────────────────────

def test_counts_dimension_from_table_rows(tmp_path: Path) -> None:
    """Markdownテーブル行から dimension を正しく抽出・集計する"""
    review_dir = tmp_path / "reviews"
    review_dir.mkdir()

    rows = [
        {"dimension": "consistency"},
        {"dimension": "consistency"},
        {"dimension": "accuracy"},
    ]
    _make_review_md(review_dir / "review-001.md", rows)

    result = scan_review_dir(review_dir)

    assert result["files_scanned"] == 1
    assert result["issue_count"] == 3

    dim_counts = {d["dimension"]: d["count"] for d in result["frequent_dimensions"] + _all_dims(result)}
    assert dim_counts.get("consistency", 0) == 2
    assert dim_counts.get("accuracy", 0) == 1


def _all_dims(result: dict) -> list[dict]:
    """frequent_dimensions に含まれない dimension は warn や別キーにあるかもしれないが
    このヘルパーは frequent_dimensions 以外の集計を補完する目的ではなく
    テストを読みやすくするために集計情報を取り出すヘルパー。
    実装次第では frequent_dimensions のみでなく全 dimension を保持してもよい。
    ただしテスト仕様は frequent_dimensions の有無のみを断言する。"""
    freq_dims = {d["dimension"] for d in result["frequent_dimensions"]}
    return [d for d in result.get("all_dimensions", []) if d["dimension"] not in freq_dims]


# ── テストケース 3: dimension フィールドのない行は skip ──────────────────────

def test_skips_non_table_lines_without_error(tmp_path: Path) -> None:
    """テーブル行以外のMarkdown（見出し、本文、別形式テーブル）は無視してエラーにならない"""
    review_dir = tmp_path / "reviews"
    review_dir.mkdir()

    content = """\
# レビュー結果

通常の本文テキスト。

| col1 | col2 |
|------|------|
| a | b |

| issue_id | file | line | dimension | problem | suggestion |
|----------|------|------|-----------|---------|------------|
| REVIEW-1-1 | foo.py | 10 | consistency | 問題1 | 修正案1 |
"""
    (review_dir / "review-001.md").write_text(content, encoding="utf-8")

    result = scan_review_dir(review_dir)

    assert result["files_scanned"] == 1
    assert result["issue_count"] == 1


# ── テストケース 4: 閾値以上の dimension が frequent_dimensions に含まれる ──

def test_includes_dimension_meeting_threshold(tmp_path: Path) -> None:
    """FREQ_THRESHOLD（3）以上のカウントを持つ dimension が frequent_dimensions に入る"""
    review_dir = tmp_path / "reviews"
    review_dir.mkdir()

    rows = [{"dimension": "consistency"}] * FREQ_THRESHOLD + [{"dimension": "accuracy"}]
    _make_review_md(review_dir / "review-001.md", rows)

    result = scan_review_dir(review_dir)

    freq_dims = [d["dimension"] for d in result["frequent_dimensions"]]
    assert "consistency" in freq_dims


# ── テストケース 5: 閾値未満の dimension は frequent_dimensions に含まれない ─

def test_excludes_dimension_below_threshold(tmp_path: Path) -> None:
    """FREQ_THRESHOLD（3）未満のカウントを持つ dimension は frequent_dimensions に入らない"""
    review_dir = tmp_path / "reviews"
    review_dir.mkdir()

    rows = [{"dimension": "accuracy"}] * (FREQ_THRESHOLD - 1)
    _make_review_md(review_dir / "review-001.md", rows)

    result = scan_review_dir(review_dir)

    freq_dims = [d["dimension"] for d in result["frequent_dimensions"]]
    assert "accuracy" not in freq_dims


# ── 複数ファイルの集約（仕様補完） ──────────────────────────────────────────

def test_aggregates_across_multiple_files(tmp_path: Path) -> None:
    """複数の .md ファイルをまたいで dimension を集計する"""
    review_dir = tmp_path / "reviews"
    review_dir.mkdir()

    _make_review_md(
        review_dir / "review-001.md",
        [{"dimension": "consistency"}] * 2,
    )
    _make_review_md(
        review_dir / "review-002.md",
        [{"dimension": "consistency"}],
    )

    result = scan_review_dir(review_dir)

    assert result["files_scanned"] == 2
    freq_dims = [d["dimension"] for d in result["frequent_dimensions"]]
    assert "consistency" in freq_dims
