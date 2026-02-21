# Claude Code コンテキストアーキテクチャ

Claude Code の4層コンテキスト機構と最適化戦略。

## コンテキスト層構造（4層）

```
読み込みタイミング: 早い ←────────────────────────→ 遅い
コンテキスト消費:   常時 ←────────────────────────→ オンデマンド

CLAUDE.md → Rules → Skills ──────────────── Subagents
  ↓          ↓        ↓                        ↓
 全体指針   パス条件  自動判断 or /slash実行   コンテキスト分離
```

> **[2025年変更]** Slash Commands（`.claude/commands/`）は Skills（`.claude/skills/`）に統合済み。`/skillname` でユーザー明示実行も可能。既存の `.claude/commands/` は後方互換性を維持して動作し続ける。

### CLAUDE.md vs Rules vs Skills（正規版比較表）

| 観点 | CLAUDE.md | Rules | Skills |
|------|-----------|-------|--------|
| 読み込みタイミング | セッション開始時 | パス条件マッチ時 | Claude判断 or `/skill` |
| コンテキスト消費 | 常時（全文） | 条件付き | 概要のみ事前、詳細は必要時 |
| 用途 | プロジェクト全体の指針 | パス固有のルール | ワークフロー・手順 |
| 適用範囲 | プロジェクト全体 | パス固有 | 明示的呼び出し or キーワード |
| 推奨サイズ | 150行以下 | 200行以下 | 500行以下 |
| 例 | 「KISSを優先」 | 「API層ではエラーログ必須」 | 「コードレビュー実施」 |

**補足**:
- Skills の呼び出しモード: デフォルトは自動＋/slash両対応。`disable-model-invocation: true` でユーザー専用（/slash のみ）、`user-invocable: false` で自動専用（/slash メニュー非表示）
- `context: fork` フロントマター: Skills から Subagent を宣言的に起動するパターン。SKILL.md の内容がサブエージェントへのプロンプトになる（`agent: Explore` 等で種別指定）
- Subagents: Task tool経由。分離コンテキスト。並列実行・専門タスク。詳細は [subagent-orchestration.md](./subagent-orchestration.md)

各層の詳細設計ガイドは個別ドキュメントを参照:
- [claude-md.md](./claude-md.md) - CLAUDE.md設計パターン
- [rules.md](./rules.md) - ルール設計ガイド
- [skills.md](./skills.md) - スキル設計・制作の完全ガイド

---

## コンテンツ配置の判断基準

### 抽出先決定テーブル

| 内容の種類 | 抽出先 | 読み込みタイミング |
|-----------|--------|------------------|
| ファイル拡張子・ディレクトリ規約 | `.claude/rules/{topic}.md` | パスアクセス時 |
| 3ステップ以上のワークフロー | `.claude/skills/{name}/SKILL.md` | Claude判断 or `/skill` |
| 副作用なし、参照情報のみ | `.claude/skills/` (`user-invocable: false`) | Claude自動判断 |
| **上記に該当しない必須情報** | **CLAUDE.mdに残す** | セッション開始時 |

### CLAUDE.mdに残すべき内容

- プロジェクト概要（1-2文）
- 基本コマンド（build, test, lint）
- 重要な参照リンク
- すべての操作に必須のルール

### 決定フロー

```
「このルールは特定のパスにのみ適用される？」
├─ YES → Rules（paths指定）
└─ NO →「複数ステップのワークフロー？」
    ├─ YES → Skills
    └─ NO → CLAUDE.mdに残す
```

---

## コンテキスト最適化戦略

### なぜskills化するのか

| 観点 | CLAUDE.mdに全部書く | skills化 |
|------|-------------------|----------|
| 起動時コンテキスト | 全文読み込み（重い） | 概要のみ（軽い） |
| 詳細情報 | 常に消費 | 必要時のみ読み込み |
| 長時間作業 | 指示を忘れがち | 自動的に再読み込み |
| 保守性 | 巨大な単一ファイル | テーマ別に分離 |

### 階層的な情報配置

```
情報の普遍性
    ↑
高  │  CLAUDE.md（全体原則）
    │      ↓
    │  Rules（パス条件付き詳細）
    │      ↓
    │  Skills（ワークフロー・手順）
    │      ↓
低  │  Subagents（分離実行タスク）
    └────────────────────────────→ コンテキスト消費
                                   低 ───────→ 高（分離）
```

### コンテキスト消費のコスト感

```
CLAUDE.md（常時ロード）      : 高コスト（全文常時）
rules/（パスマッチ時）        : 中コスト（条件付き）
skills/ descriptions のみ    : 低コスト（~100 tokens/スキル）
skills/ 本文（呼び出し時）    : 中コスト（<5k tokens）
skills/ references/（必要時） : ほぼゼロ（読み込まれるまでトークン消費なし）
```

### /compact vs /clear の使い分け

| コマンド | 動作 | 使うタイミング |
|---------|------|--------------|
| `/compact` | 会話履歴をサマリーに圧縮 | 同じ作業を続けながらコンテキストを削減（使用率 70% 目安） |
| `/clear` | 会話履歴を完全リセット | 全く別タスクへ切り替える時、コミット完了後 |

### トークン節約のチェックリスト

- [ ] CLAUDE.md は150行以下か
- [ ] パス固有のルールは Rules に分離したか
- [ ] ワークフローは Skills に分離したか
- [ ] Skills の詳細は `references/` に分離したか
- [ ] 並列実行可能なタスクは Subagents を使用しているか
- [ ] コンテキスト使用率 70% 到達前に `/compact` を検討したか

---

## 最適化ワークフロー（6段階プロセス）

```
1. 分析 → 2. 分類 → 3. 計画 → 4. 抽出 → 5. リファクタ → 6. 報告
```

1. **分析**: CLAUDE.mdの行数とセクション構造を確認
2. **分類**: 抽出先決定テーブルで各セクションの抽出先を決定
3. **計画**: 抽出計画をユーザーに提示し承認を得る
4. **抽出**: 適切なフロントマターでファイル作成
5. **リファクタ**: CLAUDE.mdを圧縮（50行未満が理想）
6. **報告**: before/after行数と作成ファイルを表示

### 効果の実例

Kent Beckの77行CLAUDE.md → **1行**に圧縮（91%削減）

```markdown
# 圧縮後
Follow Kent Beck's Test-Driven Development methodology (tdd skill) as the preferred approach for all development work.
```

### ケーススタディ: context-optimizer スキル

```
.claude/skills/context-optimizer/
├── SKILL.md              # 概要・ワークフロー
├── references/
│   ├── decision-table.md  # 判断基準テーブル
│   └── templates.md       # 各種テンプレート
└── scripts/
    └── measure-context.py # コンテキスト使用率計測
```

上記6段階プロセスをスキル化した実装例。SKILL.mdは概要のみ、詳細はreferences/に分離し、Progressive Disclosureパターンに従う。

---

## MCP との連携

### Skills vs MCP

| 観点 | Skills | MCP |
|------|--------|-----|
| 提供するもの | 知識・手順（How） | ツール・データソース（What） |
| 方向性 | 指示 → Claude | 外部システム → Claude |
| 例 | クエリパターンの指導 | データベース接続 |

### 連携パターン

```yaml
# MCP: データベース接続を提供
# mcpServers.postgres が DB 接続を提供

# Skill: クエリパターンを指導
---
name: db-query-patterns
description: データベースクエリのベストプラクティス
---

# DB クエリパターン

## 命名規則
- テーブル名: snake_case、複数形
- カラム名: snake_case
```

### MCP スコープ（2025年名称変更）

| 旧称 | 新称 | 保存先 | 用途 |
|------|------|--------|------|
| `global` | `user` | `~/.claude/mcp_servers.json` | ユーザー全体 |
| `project` | `local` | `.claude/mcp_servers.json` | プロジェクト固有・非共有 |
| (新) | `project` | `.mcp.json` | プロジェクト固有・バージョン管理対象 |

> **注意**: 旧称 `project` は新称 `local` に変更。バージョン管理共有用の新スコープ `project`（`.mcp.json`）が追加。

### MCP Tool Search（新機能）

MCP サーバーが多数存在する場合、ツール説明がコンテキストウィンドウの 10% を超えると自動的に有効化:

```bash
ENABLE_TOOL_SEARCH=auto    # デフォルト（10%超で自動有効）
ENABLE_TOOL_SEARCH=auto:5  # カスタム閾値5%
ENABLE_TOOL_SEARCH=true    # 常に有効
ENABLE_TOOL_SEARCH=false   # 無効
```

---

## 参考資料

### 公式ドキュメント
- [Claude Code Skills](https://code.claude.com/docs/en/skills) - Skills 仕様・フロントマター全フィールド
- [Custom subagents](https://code.claude.com/docs/en/sub-agents) - サブエージェント設計
- [MCP integration](https://code.claude.com/docs/en/mcp) - MCP スコープ・Tool Search
- [Claude Code: Best practices for agentic coding](https://www.anthropic.com/engineering/claude-code-best-practices)

### コミュニティ記事
- [Stop Bloating Your CLAUDE.md: Progressive Disclosure for AI Coding Tools](https://alexop.dev/posts/stop-bloating-your-claude-md-progressive-disclosure-ai-coding-tools/) (2025) - 54%トークン削減の実測値
- [Tidy Tidy Tidy! Claude Codeの設定最適化ルールを作ったら、Kent BeckのCLAUDE.mdを1プロンプトで10行、追加手直しで1行にできた](https://blog.atusy.net/2025/12/17/minimizing-claude-md/)
- [Claude Code customization guide](https://marioottmann.com/articles/claude-code-customization-guide)
- [nwiizo/workspace_2026 - memory_optimizer](https://github.com/nwiizo/workspace_2026/tree/main/tools/memory_optimizer)
