---
paths: .claude-global/**
---

# .claude-global 概要

`.claude-global/`は**ユーザー環境全体**で適用されるClaude Codeのグローバル設定を管理。

## ディレクトリ構造

```
.claude-global/
├── settings.json              # Claude Code動作設定
├── CLAUDE.md                  # Claudeへの行動指針
├── hooks/                     # イベント駆動の自動処理
├── skills/                    # 対話的ワークフロー定義
│   └── _shared/               # 共有リソース
├── agents/                    # カスタムサブエージェント
├── statusline.sh              # ステータスライン表示
├── file-suggestion.sh         # @参照時のファイル候補
└── setup-mcp.sh               # MCPサーバー初期化
```

## 各設定の責務

| カテゴリ | ファイル | 責務 |
|---------|---------|------|
| 動作設定 | `settings.json` | permissions、hooks登録、UI、MCP |
| 行動指針 | `CLAUDE.md` | コーディング哲学、環境制約 |
| 自動処理 | `hooks/` | イベント駆動実行 |
| ワークフロー | `skills/` | 対話的タスク定義 |
| サブエージェント | `agents/` | コンテキスト分離実行 |
| 動的情報 | `*.sh` | 状態取得スクリプト |

## hooks vs skills

| 判断基準 | hooks | skills |
|---------|-------|--------|
| 実行タイミング | イベント発生時に自動 | ユーザー依頼またはClaude判断 |
| ユーザー介入 | なし | あり（対話・相談） |

## 命名規則

- **全体**: kebab-case（例: `config-optimizer`, `setup-mcp.sh`）
- **frontmatter**: ハイフンケース、64文字以内

## 参考資料

- 設定ベストプラクティス: `skills/_shared/guides/config-best-practices.md`
- タスク分解ガイド: `skills/_shared/guides/task-breakdown.md`
- エージェント設計: `skills/_shared/guides/code-review-agents.md`
