# Claude Code コンテキストアーキテクチャ

Claude Code の5層コンテキスト機構と最適化戦略。

## 5層コンテキスト機構

```
読み込みタイミング: 早い ←────────────────────────→ 遅い
コンテキスト消費:   常時 ←────────────────────────→ オンデマンド

CLAUDE.md → Rules → Skills → Slash Commands → Subagents
  ↓          ↓        ↓           ↓              ↓
 全体指針   パス条件  自動判断    明示的実行    コンテキスト分離
```

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
- Slash Commands: ユーザー明示実行のみ。実行時のみコンテキスト消費。定型プロンプトショートカット
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

### トークン節約のチェックリスト

- [ ] CLAUDE.md は150行以下か
- [ ] パス固有のルールは Rules に分離したか
- [ ] ワークフローは Skills に分離したか
- [ ] Skills の詳細は `references/` に分離したか
- [ ] 並列実行可能なタスクは Subagents を使用しているか

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

---

## 参考資料

### 公式ドキュメント
- [Skill authoring best practices](https://platform.claude.com/docs/en/agents-and-tools/agent-skills/best-practices) - Skills作成ベストプラクティス
- [Agent Skills - Claude Code Docs](https://code.claude.com/docs/en/skills)
- [Claude Code: Best practices for agentic coding](https://www.anthropic.com/engineering/claude-code-best-practices)

### コミュニティ記事
- [Tidy Tidy Tidy! Claude Codeの設定最適化ルールを作ったら、Kent BeckのCLAUDE.mdを1プロンプトで10行、追加手直しで1行にできた](https://blog.atusy.net/2025/12/17/minimizing-claude-md/)
- [nwiizo/workspace_2026 - memory_optimizer](https://github.com/nwiizo/workspace_2026/tree/main/tools/memory_optimizer)
- [Claude Code customization guide](https://marioottmann.com/articles/claude-code-customization-guide)
