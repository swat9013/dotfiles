"""collect_review_files() / _list_session_dirs() / _get_repo_root() のユニットテスト。

仕様:
- ~/.claude/tmp/review/ が存在しない場合は空リストを返す
- .md ファイルが存在する場合はソートされたパス文字列リストを返す
- .md 以外のファイルのみの場合は空リストを返す
- _list_session_dirs: encoded_root 完全一致ディレクトリを含む
- _list_session_dirs: encoded_root + "-" プレフィックス一致ディレクトリを含む
- _list_session_dirs: 無関係なディレクトリは含まない
- _list_session_dirs: ~/.claude/projects/ が存在しない場合は空リストを返す
- _get_repo_root: subprocess.CalledProcessError 発生時は cwd を返す
"""

import re
import subprocess
from pathlib import Path
from unittest.mock import patch

from collect import collect_review_files, _list_session_dirs, _get_repo_root


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


# --- _list_session_dirs のテスト ---


def _make_projects_dir(tmp_path: Path) -> tuple[Path, str]:
    """projects_dir を作り、tmp_path の encoded_root を返すヘルパー。"""
    projects_dir = tmp_path / ".claude" / "projects"
    projects_dir.mkdir(parents=True)
    encoded_root = re.sub(r"[^a-zA-Z0-9]", "-", str(tmp_path))
    return projects_dir, encoded_root


def test_list_session_dirs_exact_match(tmp_path):
    """encoded_root に完全一致するディレクトリが結果に含まれる。"""
    projects_dir, encoded_root = _make_projects_dir(tmp_path)
    (projects_dir / encoded_root).mkdir()

    with patch("collect.Path.home", return_value=tmp_path):
        result = _list_session_dirs(tmp_path)

    assert any(d.name == encoded_root for d in result)


def test_list_session_dirs_prefix_match(tmp_path):
    """encoded_root + '-' プレフィックス一致のディレクトリが結果に含まれる。"""
    projects_dir, encoded_root = _make_projects_dir(tmp_path)
    (projects_dir / (encoded_root + "-worktree-branch")).mkdir()

    with patch("collect.Path.home", return_value=tmp_path):
        result = _list_session_dirs(tmp_path)

    assert any(d.name == encoded_root + "-worktree-branch" for d in result)


def test_list_session_dirs_excludes_unrelated(tmp_path):
    """無関係なディレクトリは結果に含まれない。"""
    projects_dir, encoded_root = _make_projects_dir(tmp_path)
    (projects_dir / encoded_root).mkdir()
    (projects_dir / "-other-project").mkdir()
    # '-' なしの続き（前方一致だが '-' で区切られていない）
    (projects_dir / (encoded_root + "extra")).mkdir()

    with patch("collect.Path.home", return_value=tmp_path):
        result = _list_session_dirs(tmp_path)

    names = {d.name for d in result}
    assert "-other-project" not in names
    assert encoded_root + "extra" not in names


def test_list_session_dirs_no_projects_dir(tmp_path):
    """~/.claude/projects/ が存在しない場合は空リストを返す。"""
    # projects_dir を作成しない
    with patch("collect.Path.home", return_value=tmp_path):
        result = _list_session_dirs(tmp_path)

    assert result == []


# --- _get_repo_root のテスト ---


def test_get_repo_root_fallback_on_git_error(tmp_path):
    """subprocess.CalledProcessError が発生した場合は cwd を返す。"""
    with patch(
        "collect.subprocess.run",
        side_effect=subprocess.CalledProcessError(1, []),
    ):
        result = _get_repo_root(tmp_path)

    assert result == tmp_path
