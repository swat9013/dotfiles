# コンテキスト最適化スキル設計ガイド

CLAUDE.mdを最小限に圧縮し、Claude Codeのコンテキスト効率を最大化するためのskills設計・実装ガイド。

## 1. 概要

### 目的

- **CLAUDE.md**: 50行未満（理想は20-30行）に圧縮
- **起動時コンテキスト消費**: 最小化
- **必要な情報**: 段階的に開示（Progressive Disclosure）

### なぜskills化するのか

| 観点 | CLAUDE.mdに全部書く | skills化 |
|------|-------------------|----------|
| 起動時コンテキスト | 全文読み込み（重い） | 概要のみ（軽い） |
| 詳細情報 | 常に消費 | 必要時のみ読み込み |
| 長時間作業 | 指示を忘れがち | 自動的に再読み込み |
| 保守性 | 巨大な単一ファイル | テーマ別に分離 |

### 効果の実例

Kent Beckの77行CLAUDE.md → **1行**に圧縮（91%削減）

```markdown
# 圧縮後
Follow Kent Beck's Test-Driven Development methodology (tdd skill) as the preferred approach for all development work.
```

---

## 2. 最適化の判断基準

### 抽出先決定テーブル

| 内容の種類 | 抽出先 | フロントマター | 読み込みタイミング |
|-----------|--------|--------------|------------------|
| ファイル拡張子・ディレクトリ規約 | `.claude/rules/{topic}.md` | `paths: {glob}` | パスアクセス時 |
| 3ステップ以上のワークフロー | `.claude/skills/{name}/SKILL.md` | `name:`, `description:` | Claude判断 or `/skill` |
| ユーザートリガーテンプレート | `.claude/commands/{name}.md` | `description:` | ユーザー明示実行時 |
| 限定ツールが必要な特殊タスク | `.claude/agents/{name}.md` | `name:`, `description:`, `allowed-tools:` | Task tool経由 |
| **上記に該当しない必須情報** | **CLAUDE.mdに残す** | — | セッション開始時 |

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
    └─ NO →「ユーザーが明示的に実行？」
        ├─ YES → Commands
        └─ NO →「限定ツールで分離実行？」
            ├─ YES → Agents
            └─ NO → CLAUDE.mdに残す
```

---

## 3. スキル構成設計

### 推奨ディレクトリ構造

```
.claude/skills/{skill-name}/
├── SKILL.md           # 概要・ワークフロー（500行以下）
├── references/        # 詳細ガイド（必要時のみ読み込み）
│   ├── checklist.md
│   └── examples.md
├── scripts/           # 実行スクリプト（読み込まず実行）
│   └── analyze.sh
└── templates/         # 出力テンプレート
    └── output.md
```

### Progressive Disclosure（段階的情報開示）

```
起動時に読み込まれる
├── SKILL.md（概要のみ）
│
必要時に読み込まれる（Read tool経由）
├── references/*.md
│
読み込まれない（実行のみ）
└── scripts/*
```

**ポイント**: SKILL.mdは概要のみ記載し、詳細は`references/`に分離することでトークンを節約。

---

## 4. 実装ワークフロー

### 6段階プロセス

```
1. 分析 → 2. 分類 → 3. 計画 → 4. 抽出 → 5. リファクタ → 6. 報告
```

#### 1. 分析（Analyze）

現在のCLAUDE.mdを確認：
- 行数をカウント
- セクション構造を把握
- 各セクションの役割を理解

#### 2. 分類（Classify）

判断基準テーブルを適用：
- 各セクションの抽出先を決定
- CLAUDE.mdに残す内容を特定

#### 3. 計画（Plan）

抽出計画をユーザーに提示：

```markdown
| セクション | 現在の行数 | 抽出先 | 理由 |
|-----------|----------|--------|------|
| コードレビュー手順 | 45行 | skills/code-review | 5ステップのワークフロー |
| TypeScript規約 | 30行 | rules/typescript.md | src/**/*.ts に限定 |
| プロジェクト概要 | 5行 | CLAUDE.mdに残す | 必須情報 |
```

#### 4. 抽出（Extract）

適切なフロントマターを付けてファイル作成：
- ディレクトリ構造に従って配置
- テンプレートを使用

#### 5. リファクタ（Refactor）

CLAUDE.mdを圧縮：
- 抽出した内容を参照に置き換え
- 50行未満を目標

#### 6. 報告（Report）

結果をレポート：

```markdown
## 最適化結果

- Before: 150行
- After: 25行
- 削減率: 83%

### 作成ファイル
- .claude/skills/code-review/SKILL.md
- .claude/rules/typescript.md
```

---

## 5. テンプレート集

### Rule テンプレート

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

### Skill テンプレート

```yaml
---
name: code-review
description: |
  5観点でコードレビューを実行。
  「コードレビュー」「レビューして」と依頼された時に使用。
allowed-tools:
  - Read
  - Grep
  - Glob
user-invocable: true
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

## 詳細
- チェックリスト: `references/checklist.md`
- 出力形式: `templates/output.md`
```

### Command テンプレート

```yaml
---
description: |
  CLAUDE.mdを最適化。
  「コンテキスト最適化」「CLAUDE.md圧縮」と依頼された時に使用。
---

# コンテキスト最適化コマンド

対象ファイル: $ARGUMENTS

## 実行内容
1. 指定されたCLAUDE.mdを分析
2. 判断基準に従って分類
3. 抽出計画を提示
4. 承認後、ファイルを作成
5. CLAUDE.mdをリファクタ
```

### Agent テンプレート

```yaml
---
name: security-reviewer
description: |
  セキュリティ観点でコードをレビュー。
  OWASP Top 10に基づいて脆弱性を検出。
allowed-tools:
  - Read
  - Grep
  - Glob
skills:
  - code-review
---

# セキュリティレビュアー

## 役割
OWASP Top 10 に基づいてセキュリティ脆弱性を検出。

## チェック項目
- インジェクション攻撃
- 認証・認可の不備
- 機密データの露出
- XSS（クロスサイトスクリプティング）
```

---

## 6. description記述ガイド

### フォーマット規則

```
「Verb+what. Use when+trigger1, trigger2, or trigger3.」
```

| 要素 | 説明 | 例 |
|------|------|-----|
| Verb+what | 何をするか（動詞で始める） | Reviews code for security issues |
| Use when | どんな時に使うか | reviewing PRs, checking code quality |

### 良い例・悪い例

| 種類 | description |
|------|-------------|
| **悪い例** | Helps with code review |
| **良い例** | Reviews code for security and performance issues. Use when reviewing PRs, checking code quality, or after major changes. |
| **悪い例** | Context optimization |
| **良い例** | Minimizes CLAUDE.md to reduce startup context. Use when CLAUDE.md exceeds 50 lines or context optimization is needed. |

### トリガーキーワードの選び方

ユーザーが自然に使う言葉を含める：

```yaml
description: |
  5観点でコードレビューを実行。
  「コードレビュー」「レビューして」「PRレビュー」と依頼された時に使用。
```

---

## 7. 実装例: context-optimizer スキル

### ディレクトリ構成

```
.claude/skills/context-optimizer/
├── SKILL.md
├── references/
│   ├── decision-table.md      # 判断基準テーブル
│   ├── templates.md           # 各種テンプレート
│   └── workflow-details.md    # ワークフロー詳細
└── scripts/
    └── measure-context.py     # コンテキスト使用率計測
```

### SKILL.md

```yaml
---
name: context-optimizer
description: |
  CLAUDE.mdを最小化しコンテキスト消費を最適化。
  「CLAUDE.md圧縮」「コンテキスト最適化」「context-optimizer」と依頼された時に使用。
allowed-tools:
  - Read
  - Write
  - Edit
  - Glob
  - Bash
user-invocable: true
---

# コンテキスト最適化

CLAUDE.mdを50行未満（理想20-30行）に圧縮し、起動時コンテキスト消費を最小化。

## ワークフロー

1. **分析**: CLAUDE.mdの行数とセクション構造を確認
2. **分類**: 判断基準テーブルで各セクションの抽出先を決定
3. **計画**: 抽出計画をユーザーに提示し承認を得る
4. **抽出**: 適切なフロントマターでファイル作成
5. **リファクタ**: CLAUDE.mdを圧縮
6. **報告**: before/after行数と作成ファイルを表示

## 判断基準

詳細: `references/decision-table.md`

| 内容 | 抽出先 |
|------|--------|
| パス固有ルール | `.claude/rules/` |
| 複数ステップワークフロー | `.claude/skills/` |
| ユーザートリガーテンプレート | `.claude/commands/` |
| 限定ツール特殊タスク | `.claude/agents/` |
| 上記以外の必須情報 | CLAUDE.mdに残す |

## 目標

- **行数**: 50行未満（理想20-30行）
- **削減率**: 80%以上を目指す
```

### references/decision-table.md

```markdown
# 判断基準テーブル詳細

## 抽出先決定フロー

### Rules（パス固有ルール）

**条件**: 特定のパス（ファイル拡張子、ディレクトリ）にのみ適用

**例**:
- TypeScript規約 → `src/**/*.ts` のみ
- テストファイル規約 → `**/*.test.ts` のみ
- API規約 → `src/api/**` のみ

**フロントマター**:
```yaml
paths:
  - "src/**/*.ts"
```

### Skills（ワークフロー）

**条件**: 3ステップ以上の手順を持つ

**例**:
- コードレビュー手順
- デプロイ手順
- バグ調査手順

**フロントマター**:
```yaml
name: skill-name
description: 説明とトリガーキーワード
```

### Commands（ユーザートリガー）

**条件**: ユーザーが明示的に実行するテンプレート

**例**:
- `/optimize` でCLAUDE.md最適化
- `/deploy` でデプロイ実行

**フロントマター**:
```yaml
description: 説明
```

### Agents（分離実行）

**条件**: 限定ツールで独立コンテキスト実行が必要

**例**:
- セキュリティレビュアー（Read, Grepのみ）
- ドキュメント生成（Write, Editのみ）

**フロントマター**:
```yaml
name: agent-name
description: 説明
allowed-tools:
  - Read
  - Grep
```
```

### scripts/measure-context.py

```python
#!/usr/bin/env python3
"""CLAUDE.mdの行数とコンテキスト使用率を計測"""

import sys
from pathlib import Path

def count_lines(file_path: str) -> int:
    """ファイルの行数をカウント"""
    path = Path(file_path)
    if not path.exists():
        return 0
    return len(path.read_text().splitlines())

def main():
    target = sys.argv[1] if len(sys.argv) > 1 else "CLAUDE.md"
    lines = count_lines(target)

    # 目標判定
    if lines <= 30:
        status = "IDEAL"
    elif lines <= 50:
        status = "GOOD"
    else:
        status = "NEEDS_OPTIMIZATION"

    print(f"File: {target}")
    print(f"Lines: {lines}")
    print(f"Status: {status}")
    print(f"Target: 50 lines (ideal: 20-30)")

if __name__ == "__main__":
    main()
```

---

## 参考資料

### 外部リソース

- [Tidy Tidy Tidy! Claude Codeの設定最適化ルールを作ったら、Kent BeckのCLAUDE.mdを1プロンプトで10行、追加手直しで1行にできた](https://blog.atusy.net/2025/12/17/minimizing-claude-md/)
- [nwiizo/workspace_2026 - memory_optimizer](https://github.com/nwiizo/workspace_2026/tree/main/tools/memory_optimizer)

### 関連ドキュメント

- [Claude Code コンテキスト最適化ガイド](./context-optimization.md) - 設定メカニズムの概念的説明
