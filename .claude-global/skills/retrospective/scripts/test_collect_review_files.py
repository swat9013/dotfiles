"""collect_review_files() のユニットテスト。

仕様:
- ~/.claude/tmp/review/ が存在しない場合は空リストを返す
- .md ファイルが存在する場合はソートされたパス文字列リストを返す
- .md 以外のファイルのみの場合は空リストを返す
"""

from pathlib import Path
from unittest.mock import patch

from collect import collect_review_files


def test_no_review_dir(tmp_path):
    """~/.claude/tmp/review/ ディレクトリが存在しない場合は空リストを返す。"""
    with patch("collect.Path.home", return_value=tmp_path):
        assert collect_review_files() == []


def test_with_md_files(tmp_path):
    """~/.claude/tmp/review/ に .md ファイルがある場合はソート済みパス文字列リストを返す。"""
    review_dir = tmp_path / ".claude" / "tmp" / "review"
    review_dir.mkdir(parents=True)
    (review_dir / "b.md").write_text("content")
    (review_dir / "a.md").write_text("content")

    with patch("collect.Path.home", return_value=tmp_path):
        result = collect_review_files()

    assert len(result) == 2
    assert result[0].endswith("a.md")
    assert result[1].endswith("b.md")


def test_non_md_files_only(tmp_path):
    """.md 以外のファイルのみの場合は空リストを返す。"""
    review_dir = tmp_path / ".claude" / "tmp" / "review"
    review_dir.mkdir(parents=True)
    (review_dir / "report.txt").write_text("content")
    (review_dir / "data.json").write_text("{}")

    with patch("collect.Path.home", return_value=tmp_path):
        assert collect_review_files() == []
