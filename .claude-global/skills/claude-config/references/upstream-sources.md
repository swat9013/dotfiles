# Upstream Sources

## 最終更新日

last-auto-update: 2026-04-17
last-known-version: 2.1.112

---

## 固定URL

researcherサブエージェントが直接WebFetchする対象。ドキュメントページの差分検出は `docs-diff.py` が担うため、ここにはCHANGELOGのみ記載。

| URL | カテゴリ |
|-----|---------|
| `https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md` | 全般（リリースノート） |

### docs-diff.py 監視対象

以下のページは `scripts/docs-diff.py` がスナップショット比較で差分検出する。追加・削除時はスクリプト内の `MONITORED_PAGES` を更新すること。

| slug | カテゴリ |
|------|---------|
| `settings` | settings |
| `hooks` | hooks |
| `sub-agents` | subagent |
| `overview` | context-architecture |
| `agent-teams` | agent-teams |
| `best-practices` | best-practices |
| `skills` | skills |

---

## 検索クエリ

| クエリ | 用途 |
|--------|------|
| `site:anthropic.com/news claude code` | 公式ブログ記事発見用 |
| `"claude code" best practices settings hooks` | 設定・hooks ベストプラクティス記事 |
| `"claude code" CLAUDE.md tips workflow` | CLAUDE.md 設計パターン・ワークフロー記事 |
| `"claude code" skills custom slash command` | スキル活用事例・設計パターン記事 |

---

## 監視対象ワークアラウンド

現在なし。発生したらこのセクションに `制約 / 理由 / 記載箇所` の表形式で追記し、修正確認後に削除する。

### 2026-04-17 に監視対象から除外

| 制約 | 理由 |
|------|------|
| `$()` コマンド置換禁止 | AI 判断で個別運用に切替。一律禁止は撤廃し、必要に応じて代替手段を選択 |
| HEREDOC禁止（`git commit`） | 同上 |
| Skill tool 連鎖不可 | 同上 |
| ToolSearch が claude-haiku-4-5 で利用不可 | 同上 |

### 2026-04-04 に監視対象から除外

| 制約 | 理由 |
|------|------|
| Agent tool 孫エージェントスポーン不可 | 公式ドキュメントに仕様として明記。ワークアラウンドではなく設計上の制約 |

### 2026-04-02 に監視対象から除外

| 制約 | 理由 |
|------|------|
| `run_in_background` + TaskOutput並列禁止 | TaskOutput 自体が v2.1.83 で非推奨化。`Read` で代替可能になり制約解消 |
| PreToolUse/PostToolUse hooks が Agent tool でバイパスされる | 仕様として確定（plugin subagents は hooks/mcpServers/permissionMode を無視）。ワークアラウンドではなく設計上の制約 |
