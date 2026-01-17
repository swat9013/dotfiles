# Claude Code コンテキスト最適化ガイド

Claude Code CLI でコンテキストを効率的に管理し、トークン消費を最小化しながら必要な情報を適切に提供する方法。

## 設定メカニズムの全体像

Claude Code は5つの設定メカニズムを提供し、それぞれ異なる役割を持つ。

```
読み込みタイミング: 早い ←────────────────────────→ 遅い
コンテキスト消費:   常時 ←────────────────────────→ オンデマンド

CLAUDE.md → Rules → Skills → Slash Commands → Subagents
  ↓          ↓        ↓           ↓              ↓
 全体指針   パス条件  自動判断    明示的実行    コンテキスト分離
```

### 設定メカニズム比較表

| メカニズム | 読み込みタイミング | コンテキスト消費 | 用途 |
|-----------|-------------------|-----------------|------|
| **CLAUDE.md** | セッション開始時 | 常時（全文） | プロジェクト全体の指針 |
| **Rules** | パス条件マッチ時 | 条件付き | パス固有のルール |
| **Skills** | Claude判断 or `/skill` | 概要のみ事前、詳細は必要時 | ワークフロー・手順 |
| **Slash Commands** | ユーザー明示実行時 | 実行時のみ | 定型プロンプト |
| **Subagents** | Task tool 経由 | 分離コンテキスト | 並列実行・専門タスク |

---

## 1. CLAUDE.md - プロジェクト全体の指針

### 役割

セッション開始時に**全文が読み込まれる**ため、常に必要な情報のみを記載。

### 配置場所

| 場所 | 用途 |
|------|------|
| `~/.claude/CLAUDE.md` | 全プロジェクト共通設定 |
| `プロジェクト/CLAUDE.md` | プロジェクト固有設定 |
| `プロジェクト/サブディレクトリ/CLAUDE.md` | サブディレクトリ固有（追記される） |

### ベストプラクティス

```markdown
# CLAUDE.md

## プロジェクト概要
[1-2文でプロジェクトの目的]

## 技術スタック
- [使用技術を箇条書き]

## 重要なコマンド
- ビルド: `npm run build`
- テスト: `npm test`

## コーディング規約
- [最重要な規約のみ]
```

**制約**:
- **150行以下**を推奨
- 詳細は Rules や Skills に分離
- 「常に必要か？」を基準に取捨選択

### アンチパターン

```markdown
❌ 500行以上の詳細ドキュメント
❌ 特定ディレクトリのみに関係するルール（→ Rulesへ）
❌ ワークフローや手順（→ Skillsへ）
❌ 外部API仕様の詳細（→ MCPやSkillsへ）
```

---

## 2. Rules - パス固有のルール

### 役割

**特定のパスにアクセスした時のみ**読み込まれる条件付きガイドライン。

### 配置場所

| 場所 | 優先度 |
|------|--------|
| `~/.claude/rules/` | 全プロジェクト共通 |
| `プロジェクト/.claude/rules/` | プロジェクト固有（優先） |

### frontmatter によるパス指定

```yaml
---
paths:
  - "src/api/**/*.ts"
  - "src/api/**/*.js"
  - "!src/api/__tests__/**"  # 除外パターン
---

# API エンドポイント規約

## 命名規則
- RESTful なリソース名を使用
- バージョンは URL に含める（/api/v1/...）

## エラーハンドリング
- 全エンドポイントで統一したエラーレスポンス形式
```

### 使い分けの判断

```
「このルールは特定のパスにのみ適用される？」
├─ YES → Rules（paths 指定）
│   例: src/api/** のみの REST 規約
│
└─ NO → CLAUDE.md
    例: 全体の KISS 原則
```

### ベストプラクティス

- **200行以下**を推奨
- 1ファイル1テーマ（SRP）
- パターンを活用（`**/*.test.ts` でテストファイル用ルール）

---

## 3. Skills - ワークフロー・手順定義

### 役割

Claude が**自動判断**または**ユーザー明示的実行**で読み込む専門知識パッケージ。

### 配置場所

| 場所 | 用途 |
|------|------|
| `~/.claude/skills/` | 全プロジェクト共通 |
| `プロジェクト/.claude/skills/` | プロジェクト固有（優先） |

### SKILL.md の構造

```yaml
---
name: code-review                    # 必須: kebab-case、64文字以内
description: |                       # 必須: トリガーキーワードを含める
  5観点でコードレビューを実行。
  「コードレビュー」「レビューして」と依頼された時に使用。
allowed-tools:                       # オプション: ツール制限
  - Read
  - Grep
  - Glob
user-invocable: true                 # オプション: /メニュー表示（デフォルト: true）
---

# コードレビュー

## ワークフロー
1. 対象ファイルを特定
2. 5観点でレビュー実行
3. 結果を構造化して出力

## 観点
- セキュリティ
- 信頼性
- パフォーマンス
- アーキテクチャ
- 品質
```

### Progressive Disclosure（段階的情報開示）

```
skill-name/
├── SKILL.md           # 概要・ワークフロー（500行以下）
├── references/        # 詳細ガイド（必要時のみ読み込み）
│   └── security-checklist.md
├── scripts/           # 実行スクリプト（読み込まず実行）
│   └── analyze.py
└── assets/            # テンプレート
    └── output-template.md
```

**ポイント**: SKILL.md は概要のみ、詳細は `references/` に分離してトークン節約。

### Skills vs Slash Commands

| 観点 | Skills | Slash Commands |
|------|--------|----------------|
| トリガー | Claude自動判断 or `/skill` | ユーザー明示のみ |
| 構造 | ディレクトリ（複数ファイル可） | 単一ファイル |
| 用途 | 専門知識パッケージ | 定型プロンプトショートカット |

---

## 4. Subagents - コンテキスト分離

### 役割

**独立したコンテキスト**で動作する専門エージェント。メイン会話のトークンを消費しない。

### Skills vs Subagents の決定木

```
「作業中に継続的な対話が必要？」
├─ YES → Skills（コンテキスト共有）
│   例: コードレビューでフィードバックを受けながら修正
│
└─ NO → 「試行錯誤や並列実行が必要？」
    ├─ YES → Subagents（コンテキスト分離）
    │   例: 5観点を並列でレビュー、調査タスク
    │
    └─ NO → Skills
```

### 組み込みエージェント

| エージェント | 用途 | モデル |
|-------------|------|--------|
| `Explore` | コードベース検索（読み取り専用） | Haiku |
| `Plan` | 実装計画の設計 | Sonnet |
| `general-purpose` | 複雑なマルチステップタスク | Sonnet |

### カスタムエージェントの定義

```yaml
# .claude/agents/security-reviewer.md
---
name: security-reviewer
description: セキュリティ観点でコードをレビュー
skills: code-review, security-check  # Skills を継承可能
model: claude-sonnet-4-20250514
---

# セキュリティレビュアー

## 役割
OWASP Top 10 に基づいてセキュリティ脆弱性を検出。

## チェック項目
- インジェクション攻撃
- 認証・認可の不備
- 機密データの露出
```

**注意**: 組み込みエージェント（Explore、Plan、general-purpose）は Skills にアクセスできない。カスタムエージェントのみ `skills` フィールドで指定可能。

---

## 5. コンテキスト最適化戦略

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

### 実装例: コードレビューの最適化

**Before（非効率）**: CLAUDE.md に500行のレビューガイドを記載

**After（最適化）**:

```
CLAUDE.md（10行）
└─ 「コードレビューは /code-review で実行」と記載

.claude/skills/code-review/
├── SKILL.md（概要・ワークフロー）
├── references/
│   ├── security-checklist.md
│   ├── performance-checklist.md
│   └── architecture-checklist.md
└── assets/
    └── output-template.md

.claude/agents/
├── security-reviewer.md
├── reliability-reviewer.md
├── performance-reviewer.md
├── architecture-reviewer.md
└── quality-reviewer.md
```

**結果**:
- セッション開始時のトークン消費: 500行 → 10行
- レビュー実行時: 必要な情報のみ段階的に読み込み
- 5観点: 並列実行で時間短縮

### トークン節約のチェックリスト

- [ ] CLAUDE.md は150行以下か
- [ ] パス固有のルールは Rules に分離したか
- [ ] ワークフローは Skills に分離したか
- [ ] Skills の詳細は `references/` に分離したか
- [ ] 並列実行可能なタスクは Subagents を使用しているか
- [ ] 組み込みエージェント（Explore）を調査タスクに活用しているか

---

## 6. MCP との連携

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

## よく使うクエリ
- ユーザー検索: `SELECT * FROM users WHERE ...`
```

---

## 参考資料

### 公式ドキュメント
- [Agent Skills - Claude Code Docs](https://code.claude.com/docs/en/skills)
- [Claude Code: Best practices for agentic coding](https://www.anthropic.com/engineering/claude-code-best-practices)

### コミュニティ記事
- [Claude Codeの Agent Skills は設定したほうがいい](https://syu-m-5151.hatenablog.com/entry/2025/12/19/173309)
- [Claude Code ユーザー設定](https://blog.atusy.net/2025/12/15/claude-code-user-config/)
- [Claude Code customization guide](https://marioottmann.com/articles/claude-code-customization-guide)

### 関連ガイド
- [How to Use Claude Code: A Guide to Slash Commands, Agents, Skills, and Plug-Ins](https://www.producttalk.org/how-to-use-claude-code-features/)
- [How I Use Every Claude Code Feature](https://blog.sshh.io/p/how-i-use-every-claude-code-feature)
