# 抽出先判断基準

CLAUDE.mdの各セクションをどこに抽出するかの詳細な判断基準。

## 判断フローチャート

```
「このコンテンツは...」
│
├─ 特定のファイルパスにのみ適用？
│   └─ YES → Rules（paths指定）
│
├─ 3ステップ以上のワークフロー？
│   └─ YES → Skills
│
├─ ユーザーが明示的に実行するテンプレート？
│   └─ YES → Commands
│
├─ 独立コンテキストで実行すべき専門タスク？
│   └─ YES → Agents
│
└─ 全操作に必須？
    └─ YES → CLAUDE.mdに残す
```

---

## 1. Rules（パス固有ルール）

### 判断基準

- 特定のファイル拡張子に適用（`.ts`, `.rs`, `.go`, `.py`など）
- 特定のディレクトリ構造に適用（`src/api/`, `tests/`など）
- 除外パターンがある（`!**/*.test.ts`）

### frontmatter

```yaml
---
paths:
  - "src/**/*.ts"
  - "!src/**/*.test.ts"
---
```

### 例

| コンテンツ | 抽出先 | paths |
|-----------|--------|-------|
| TypeScript型付け規約 | `rules/typescript.md` | `**/*.ts` |
| APIエンドポイント設計 | `rules/api-design.md` | `src/api/**` |
| テストファイル規約 | `rules/testing.md` | `**/*.test.ts`, `**/*.spec.ts` |
| Rust安全性ルール | `rules/rust-safety.md` | `**/*.rs` |

---

## 2. Skills（ワークフロー）

### 判断基準

- 3ステップ以上の手順がある
- Claude が自動判断で適用可能
- 「〜して」で依頼されるタスク

### frontmatter

```yaml
---
name: skill-name
description: |
  説明文。
  「〜時」「〜したい時」に使用。
allowed-tools:          # オプション
  - Read
  - Grep
---
```

### 例

| コンテンツ | 抽出先 | トリガー |
|-----------|--------|----------|
| コードレビュー手順 | `skills/code-review/SKILL.md` | 「レビューして」 |
| リファクタリングガイド | `skills/refactor/SKILL.md` | 「リファクタして」 |
| デバッグ手順 | `skills/debug/SKILL.md` | 「デバッグして」 |
| ドキュメント生成 | `skills/docs/SKILL.md` | 「ドキュメント作成して」 |

---

## 3. Commands（定型プロンプト）

### 判断基準

- ユーザーが `/command` で明示的に実行
- 引数を受け取るテンプレート
- 単発の定型タスク

### frontmatter

```yaml
---
description: 簡潔な説明 (project)
---
```

### 例

| コンテンツ | 抽出先 | 使用方法 |
|-----------|--------|----------|
| コミット作成 | `commands/commit.md` | `/commit [message]` |
| PR作成 | `commands/pr.md` | `/pr [title]` |
| Issue作成 | `commands/issue.md` | `/issue [title]` |

---

## 4. Agents（専門タスク）

### 判断基準

- 独立したコンテキストで実行すべき
- 特定のツールセットに限定
- 並列実行の可能性がある

### frontmatter

```yaml
---
name: agent-name
description: |
  説明文。
  〜時に使用。
allowed-tools:
  - Read
  - Grep
skills:               # オプション: 継承するスキル
  - code-review
---
```

### 例

| コンテンツ | 抽出先 | 用途 |
|-----------|--------|------|
| セキュリティレビュー | `agents/security-reviewer.md` | 脆弱性検出 |
| パフォーマンス分析 | `agents/perf-analyzer.md` | ボトルネック特定 |
| 依存関係監査 | `agents/dep-auditor.md` | 依存関係の問題検出 |

---

## 5. CLAUDE.mdに残すべき内容

### 判断基準

- **全ての操作に必須**
- プロジェクトの本質的な情報
- Claudeの基本的な振る舞いを決定

### 残す内容

1. **プロジェクト概要**（1-2文）
   - 何のためのプロジェクトか
   - 主な目的

2. **技術スタック**（箇条書き）
   - 言語
   - フレームワーク
   - 主要ライブラリ

3. **重要コマンド**（最重要のみ）
   - ビルド
   - テスト
   - lint

4. **核心的コーディング原則**（3-5項目）
   - KISS/DRY/SLAP/SRP
   - プロジェクト固有の制約

5. **参照**（必要な場合のみ）
   - 「詳細は /skill-name で」

---

## 迷った時の判断

| 状況 | 判断 |
|------|------|
| パス条件があるが手順もある | → Skills（手順優先） |
| 手順があるがユーザー実行前提 | → Commands |
| 複数の観点で並列実行したい | → Agents |
| どこにも当てはまらない | → CLAUDE.mdに残す |
