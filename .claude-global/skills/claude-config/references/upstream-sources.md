# Upstream Sources

## 最終更新日

last-auto-update: 2026-03-29T16:20

---

## 固定URL

| URL | カテゴリ |
|-----|---------|
| `https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md` | 全般（リリースノート） |
| `https://code.claude.com/docs/en/settings` | settings |
| `https://code.claude.com/docs/en/hooks` | hooks |
| `https://code.claude.com/docs/en/sub-agents` | subagent |
| `https://code.claude.com/docs/en/overview` | context-architecture |
| `https://code.claude.com/docs/en/agent-teams` | agent-teams |

---

## 検索クエリ

| クエリ | 用途 |
|--------|------|
| `site:anthropic.com/news claude code` | ブログ記事発見用 |

---

## 監視対象ワークアラウンド

バグ/未実装で回避中。修正されたら解除可能。

| 制約 | 理由 | 記載箇所 |
|------|------|---------|
| `$()` コマンド置換禁止 | サブシェルごとにパーミッション確認が発生 | CLAUDE.md |
| HEREDOC禁止（`git commit`） | パーミッションプロンプト誘発 | CLAUDE.md |
| Agent tool `run_in_background` + TaskOutput並列禁止 | Sibling error で全失敗 | Gotchas |
| Agent tool 孫エージェントスポーン不可 | 1段階のみ | Gotchas |
| PreToolUse/PostToolUse hooks が Agent tool でバイパスされる | subagent frontmatter での hooks 定義で部分対応済。plugin subagent は hooks frontmatter 無視 | Gotchas |
| Skill tool 連鎖不可 | 呼び出しが失敗 | Gotchas |
| ToolSearch が claude-haiku-4-5 で利用不可 | tool_reference blocks 非対応 | Gotchas |

修正確認後: 対応ワークアラウンドを記載箇所から削除し、このテーブルから行を除去する。
