---
paths: .claude-global/**
---

# .claude-global 概要

`.claude-global/`は**ユーザー環境全体**で適用されるClaude Codeのグローバル設定を管理。

## ディレクトリ構造

```
.claude-global/
├── settings.json                # Claude Code動作設定
├── settings.local.json.template # ローカル設定テンプレート
├── CLAUDE.md                    # Claudeへの行動指針
├── rules/                       # パス固有のガイドライン
├── hooks/                       # イベント駆動の自動処理
├── skills/                      # 対話的ワークフロー定義
├── statusline.sh                # ステータスライン表示
├── file-suggestion.sh           # @参照時のファイル候補
├── setup-mcp.sh                 # MCPサーバー初期化
└── setup-mcp-local.sh.example   # ローカルMCP設定テンプレート
```

## 各設定の責務

| カテゴリ | ファイル | 責務 |
|---------|---------|------|
| 動作設定 | `settings.json` | permissions、hooks登録、UI、MCP |
| 行動指針 | `CLAUDE.md` | コーディング哲学、環境制約 |
| ガイドライン | `rules/` | パス固有のルール |
| 自動処理 | `hooks/` | イベント駆動実行（SessionStart, PreToolUse, PostToolUse, Stop 等） |
| ワークフロー | `skills/` | 対話的タスク定義（サブエージェント呼び出し含む） |
| 動的情報 | `*.sh` | 状態取得スクリプト |

## hooks vs skills

| 判断基準 | hooks | skills |
|---------|-------|--------|
| 実行タイミング | イベント発生時に自動 | ユーザー依頼またはClaude判断 |
| ユーザー介入 | なし | あり（対話・相談） |

## 情報階層とコスト

| 階層 | 読み込みタイミング | コスト |
|-----|------------------|-------|
| CLAUDE.md | 常時 | 高 |
| rules/ | パスマッチ時（※#16299で全件ロード） | 中 |
| skills/ | 必要時のみ | 低 |
| Subagents | Task tool経由 | 分離（トークン消費なし） |

### 配置判断
- 「この情報は常に必要か？」→ No なら下位層へ
- 使用頻度が低い情報は skills/ へ
- サブエージェントが必要な場合は skills/ 内でTask toolを呼び出す

## パス参照の規則

スキル・hooks・スクリプト内でのパス参照は用途によって使い分ける:

| 用途 | パス形式 | 理由 |
|------|----------|------|
| ドキュメント参照 | `~/.claude/skills/...` | シンボリックリンク経由で環境非依存 |
| スクリプト実行 | `~/.dotfiles/.claude-global/...` | シェル実行時の確実性 |
| settings.json command | `~/.dotfiles/.claude-global/...` | 実ファイルパス必須 |

## DRY原則

- **禁止**: rules/ と CLAUDE.md に同じ指示を重複記載
- **禁止**: 複数スキルに同一のreferences/を個別コピー → `_shared/` に集約
- **推奨**: 共通のアクション指向ルールは上位層（CLAUDE.md）に集約し、パス固有の補足のみrules/に記載

## 命名規則

- **全体**: kebab-case（例: `context-optimizer`, `setup-mcp.sh`）
- **frontmatter**: ハイフンケース、64文字以内
