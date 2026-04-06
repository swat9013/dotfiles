#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
# skills配下の各SKILL.mdをclaude -pで個別レビュー・修正する
#
# Usage: review-skills.py [-n ITERATIONS] [-p PARALLEL] [-m MODEL] [SKILL_NAME]
#   -n  レビュー回数（デフォルト: 3）
#   -p  並列数（デフォルト: 3）
#   -m  モデル（デフォルト: opus）
#   SKILL_NAME  特定スキルのみ対象（省略で全スキル）

import argparse
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from pathlib import Path

SKILLS_DIR = Path.home() / ".dotfiles/.claude-global/skills"

REVIEW_PROMPT = """\
あなたはClaude Codeスキルの品質レビュアーです。

## レビュー対象
以下のSKILL.mdファイルをレビューし、問題があれば直接修正してください。

対象ファイル: {{SKILL_PATH}}

## レビュー基準

Read ~/.dotfiles/.claude-global/skills/claude-config/references/skill-design-patterns.md の設計パターンをレビュー基準として使用してください。

## 修正ルール
- 問題があればEditツールで直接修正する
- 修正不要なら「修正不要」とだけ出力する
- references/やtemplates/配下のファイルも必要に応じて読み、整合性を確認する
- 既存の構造や設計意図を尊重し、意味を変えない範囲で改善する
- descriptionのトリガーキーワードは、ユーザーが自然に使う言葉を網羅する

## 出力形式
修正した場合:
[修正] 修正内容の1行要約
修正不要の場合:
[OK] 修正不要\
"""

EXCLUDE_DIRS = {"skill-design-patterns", "_shared", "scripts"}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="skills配下の各SKILL.mdをclaude -pで個別レビュー・修正する"
    )
    parser.add_argument("-n", dest="iterations", type=int, default=3, metavar="ITERATIONS", help="レビュー回数（デフォルト: 3）")
    parser.add_argument("-p", dest="parallel", type=int, default=3, metavar="PARALLEL", help="並列数（デフォルト: 3）")
    parser.add_argument("-m", dest="model", default="opus", metavar="MODEL", help="モデル（デフォルト: opus）")
    parser.add_argument("skill_name", nargs="?", default=None, help="特定スキルのみ対象（省略で全スキル）")
    return parser.parse_args()


def collect_skill_dirs(target_skill: str | None) -> list[Path]:
    if target_skill:
        skill_dir = SKILLS_DIR / target_skill
        if not skill_dir.is_dir():
            print(f"エラー: スキル '{target_skill}' が見つかりません", file=sys.stderr)
            sys.exit(1)
        return [skill_dir]

    return sorted(
        d for d in SKILLS_DIR.iterdir()
        if d.is_dir()
        and d.name not in EXCLUDE_DIRS
        and (d / "SKILL.md").exists()
    )


def review_skill(skill_dir: Path, iter_num: int, log_dir: Path, model: str) -> tuple[str, str]:
    skill_name = skill_dir.name
    skill_path = skill_dir / "SKILL.md"
    log_file = log_dir / f"{skill_name}_iter{iter_num}.log"

    prompt = REVIEW_PROMPT.replace("{{SKILL_PATH}}", str(skill_path))

    result = subprocess.run(
        [
            "claude", "-p", prompt,
            "--allowedTools", "Read,Edit,Glob,Grep",
            "--model", model,
            "--add-dir", str(SKILLS_DIR),
        ],
        capture_output=True,
        text=True,
    )

    log_file.write_text(result.stdout + result.stderr)

    if result.returncode == 0:
        match = re.search(r"^\[(修正|OK)\].*", result.stdout, re.MULTILINE)
        outcome = match.group(0) if match else "[?] 結果不明"
    else:
        outcome = "claude実行失敗"

    return skill_name, outcome


def run_iteration(
    iter_num: int,
    total_iters: int,
    skill_dirs: list[Path],
    parallel: int,
    log_dir: Path,
    model: str,
) -> None:
    total = len(skill_dirs)
    print(f"--- Iteration {iter_num}/{total_iters} ---")

    for batch_start in range(0, total, parallel):
        batch = skill_dirs[batch_start: batch_start + parallel]

        for idx, skill_dir in enumerate(batch, start=batch_start + 1):
            print(f"[{idx}/{total}] {skill_dir.name} (iter {iter_num}) 開始...")

        results: dict[str, str] = {}
        with ThreadPoolExecutor(max_workers=parallel) as executor:
            futures = {
                executor.submit(review_skill, skill_dir, iter_num, log_dir, model): skill_dir.name
                for skill_dir in batch
            }
            for future in as_completed(futures):
                skill_name, outcome = future.result()
                results[skill_name] = outcome

        for skill_dir in batch:
            outcome = results.get(skill_dir.name, "[?] 結果不明")
            print(f"  [完了] {skill_dir.name}: {outcome}")

    print()


def main() -> None:
    args = parse_args()
    skill_dirs = collect_skill_dirs(args.skill_name)
    total = len(skill_dirs)

    timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
    script_dir = Path(__file__).parent
    project_root = script_dir.parent.parent.parent
    log_dir = project_root / "tmp" / f"review-skills-{timestamp}"
    log_dir.mkdir(parents=True, exist_ok=True)

    print("=== Skills Review ===")
    print(f"対象: {total}スキル × {args.iterations}回（並列: {args.parallel}, モデル: {args.model}）")
    print(f"ログ: {log_dir}")
    print()

    try:
        for iter_num in range(1, args.iterations + 1):
            run_iteration(iter_num, args.iterations, skill_dirs, args.parallel, log_dir, args.model)
    except KeyboardInterrupt:
        print()
        print("中断: 子プロセスを停止中...")
        print("停止完了")
        sys.exit(130)

    print("=== 完了 ===")
    print(f"ログディレクトリ: {log_dir}")


if __name__ == "__main__":
    main()
