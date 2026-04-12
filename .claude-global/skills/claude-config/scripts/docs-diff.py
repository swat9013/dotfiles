#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "httpx<1",
#     "html2text<2025",
# ]
# ///
"""監視対象ドキュメントをfetchし、スナップショットとのdiffを出力する。"""

import argparse
import difflib
import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

import html2text
import httpx

SITEMAP_URL = "https://code.claude.com/docs/sitemap.xml"
BASE_URL = "https://code.claude.com/docs/en"

MONITORED_PAGES: dict[str, str] = {
    "settings": "settings",
    "hooks": "hooks",
    "sub-agents": "subagent",
    "overview": "context-architecture",
    "agent-teams": "agent-teams",
    "best-practices": "best-practices",
    "skills": "skills",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--snapshot-dir",
        type=Path,
        required=True,
        help="スナップショット保存ディレクトリ",
    )
    parser.add_argument(
        "--update-snapshots",
        action="store_true",
        help="diff出力後にスナップショットを現在の内容で更新する",
    )
    return parser.parse_args()


def fetch_sitemap_lastmods(client: httpx.Client) -> dict[str, str]:
    """sitemap.xml から /docs/en/ ページの lastmod を取得する。"""
    resp = client.get(SITEMAP_URL)
    resp.raise_for_status()

    root = ET.fromstring(resp.text)
    ns = {"sm": "http://www.sitemaps.org/schemas/sitemap/0.9"}

    lastmods: dict[str, str] = {}
    for url_elem in root.findall(".//sm:url", ns):
        loc = url_elem.findtext("sm:loc", "", ns)
        lastmod = url_elem.findtext("sm:lastmod", "", ns)
        if "/docs/en/" in loc and lastmod:
            slug = loc.rstrip("/").split("/docs/en/")[-1]
            lastmods[slug] = lastmod
    return lastmods


def extract_main_content(html: str) -> str:
    """HTMLからメインコンテンツをmarkdownとして抽出する。

    ナビゲーション・フッター等のボイラープレートを除外し、
    diff安定性のために正規化する。
    """
    h = html2text.HTML2Text()
    h.ignore_links = False
    h.ignore_images = True
    h.body_width = 0
    h.ignore_emphasis = False
    h.skip_internal_links = True
    text = h.handle(html)

    # Cloudflare email-protection のhexトークンはfetchごとに変わるため固定値に置換
    text = re.sub(
        r"/cdn-cgi/l/email-protection#[0-9a-fA-F]+",
        "/cdn-cgi/l/email-protection#REDACTED",
        text,
    )

    lines = text.splitlines()

    # 最初の H1 見出しを探してそこから開始（ナビゲーション除外）
    start = 0
    for i, line in enumerate(lines):
        if line.startswith("# "):
            start = i
            break

    # 末尾のフッターパターンを除外
    end = len(lines)
    footer_patterns = [
        re.compile(r"^Was this page helpful"),
        re.compile(r"^On this page$"),
        re.compile(r"^\[.*\]\(/docs/en/"),
    ]
    for i in range(len(lines) - 1, start, -1):
        if any(p.match(lines[i]) for p in footer_patterns):
            end = i
        elif lines[i].strip():
            break

    content_lines = lines[start:end]

    # 連続空行を1行に正規化
    result: list[str] = []
    prev_blank = False
    for line in content_lines:
        stripped = line.rstrip()
        if not stripped:
            if not prev_blank:
                result.append("")
            prev_blank = True
        else:
            result.append(stripped)
            prev_blank = False

    return "\n".join(result).strip() + "\n"


def fetch_page(client: httpx.Client, slug: str) -> str:
    """ドキュメントページを取得し、クリーンなmarkdownコンテンツを返す。"""
    url = f"{BASE_URL}/{slug}"
    resp = client.get(url)
    resp.raise_for_status()
    return extract_main_content(resp.text)


def generate_diff(old_content: str, new_content: str, slug: str) -> str | None:
    """unified diff を生成する。変更なしならNone。"""
    old_lines = old_content.splitlines(keepends=True)
    new_lines = new_content.splitlines(keepends=True)

    diff_lines = list(
        difflib.unified_diff(
            old_lines,
            new_lines,
            fromfile=f"snapshot/{slug}.md",
            tofile=f"current/{slug}.md",
            n=3,
        )
    )
    if not diff_lines:
        return None
    return "".join(diff_lines)


def main() -> int:
    args = parse_args()
    snapshot_dir: Path = args.snapshot_dir
    snapshot_dir.mkdir(parents=True, exist_ok=True)

    changed: list[dict] = []
    unchanged: list[dict] = []
    new_pages: list[dict] = []
    errors: list[dict] = []

    with httpx.Client(follow_redirects=True, timeout=30) as client:
        lastmods = fetch_sitemap_lastmods(client)

        for slug, category in MONITORED_PAGES.items():
            lastmod = lastmods.get(slug, "unknown")
            snapshot_file = snapshot_dir / f"{slug}.md"

            try:
                current = fetch_page(client, slug)
            except httpx.HTTPError as e:
                errors.append({"slug": slug, "category": category, "error": str(e)})
                continue

            if snapshot_file.exists():
                previous = snapshot_file.read_text()
                diff = generate_diff(previous, current, slug)
                entry = {"slug": slug, "category": category, "lastmod": lastmod}
                if diff:
                    entry["diff"] = diff
                    changed.append(entry)
                else:
                    unchanged.append(entry)
            else:
                new_pages.append({"slug": slug, "category": category, "lastmod": lastmod})

            if args.update_snapshots:
                snapshot_file.write_text(current)

    # --- 出力 ---
    total_changed = len(changed) + len(new_pages)

    if total_changed == 0 and not errors:
        print("RESULT: NO_CHANGES")
        print()
        for r in unchanged:
            print(f"  {r['slug']} ({r['category']}): unchanged (lastmod: {r['lastmod']})")
        return 0

    print(f"RESULT: CHANGES_FOUND ({total_changed} pages)")
    print()

    for r in changed:
        print(f"### {r['slug']} ({r['category']}) — lastmod: {r['lastmod']}")
        print()
        print("```diff")
        print(r["diff"], end="")
        print("```")
        print()

    for r in new_pages:
        print(f"### {r['slug']} ({r['category']}) — lastmod: {r['lastmod']}")
        print("Status: NEW (スナップショットなし — 初回は --update-snapshots で作成)")
        print()

    if unchanged:
        print("### Unchanged")
        for r in unchanged:
            print(f"  - {r['slug']} (lastmod: {r['lastmod']})")
        print()

    if errors:
        print("### Errors")
        for r in errors:
            print(f"  - {r['slug']}: {r['error']}")
        print()

    if args.update_snapshots:
        print(f"Snapshots updated: {snapshot_dir}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
