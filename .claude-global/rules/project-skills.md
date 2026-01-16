---
paths: "**/.claude/skills/**"
---

# Skills 作成ガイド

`.claude/skills/` 配下のスキルを作成・編集する際のガイドライン。

## Skills とは

Claudeが実行するワークフローや手順を定義した再利用可能なコンポーネント。明示的な呼び出し（`/skill-name`）またはキーワードマッチで起動する。

## ディレクトリ構成

```
skill-name/
├── SKILL.md           # 必須: 手順・ガイドライン
├── references/        # 参照ドキュメント（必要時のみ読み込み）
├── scripts/           # 実行スクリプト（読み込まず実行）
└── assets/            # テンプレート・画像等
```

## SKILL.md 制約

- **上限**: 500行
- **形式**: YAML frontmatter + Markdown本文
- **Progressive Disclosure**: 詳細は `references/` に分離

## frontmatter 仕様

```yaml
---
name: my-skill
description: |
  スキルの説明。トリガーキーワードを含める。
  「設計して」「plan.md作成」と依頼された時に使用。

# ツール制限（オプション）
allowed-tools:
  - Read
  - Glob
  - Grep
  - Task
  - WebSearch

# ツール除外（オプション）
denied-tools:
  - Edit
  - Write

# 自動起動を無効化（オプション）
user-invocable-only: true
---
```

### description 設計

**必須要素**:
- スキルの目的（1行目）
- トリガーキーワード（「〜と依頼されたら」形式）

**良い例**:
```yaml
description: |
  システム設計・アーキテクチャ評価を担当する専門エージェント。
  「設計して」「アーキテクチャを考えて」「plan.md作成」と依頼された時に使用。
```

**悪い例**:
```yaml
description: "設計用のスキル"  # トリガー不明、用途曖昧
```

## skills vs agents vs rules

| 観点 | skills/ | agents/ | rules/ |
|------|---------|---------|--------|
| 用途 | ワークフロー・手順 | 並列実行・専門役割 | パス固有のルール |
| 実行形態 | 対話型・相談型 | コンテキスト分離 | 自動適用 |
| トリガー | Claude自動選択 or `/name` | Task tool経由 | ディレクトリアクセス時 |
| コンテキスト | 共有（会話継続） | 分離（独立実行） | - |

### skills vs agents 判断基準

| 判断基準 | skills | agents |
|---------|--------|--------|
| 並列実行が必要? | No → skills | Yes → agents |
| 試行錯誤が必要? | No → skills | Yes → agents |
| 専門的な役割分担? | No → skills | Yes → agents |
| 対話しながら進める? | Yes → skills | No → agents |

## SKILL.md 構成テンプレート

```markdown
---
name: example-skill
description: |
  スキルの説明とトリガーキーワード。
---

# スキル名

## 概要
何をするスキルか（1-2文）

## 前提条件
- 必要な情報
- 事前準備

## 手順

### Phase 1: 調査
1. ステップ1
2. ステップ2

### Phase 2: 実行
1. ステップ3
2. ステップ4

## 出力形式
期待される成果物の形式

## 参照
- `references/detail.md` - 詳細ガイド
```

## Progressive Disclosure

SKILL.mdは概要と手順に集中し、詳細は分離する。

| ファイル | 内容 | 読み込みタイミング |
|---------|------|------------------|
| SKILL.md | 概要、手順、判断基準 | スキル起動時 |
| references/ | 詳細ガイド、仕様 | 必要時のみ |
| scripts/ | 実行スクリプト | 実行時のみ |
| assets/ | テンプレート | 必要時のみ |

## アンチパターン

| パターン | 問題点 | 対策 |
|---------|--------|------|
| 500行超のSKILL.md | 初期読み込みコスト大 | references/に分離 |
| トリガーキーワードなし | 自動選択されない | descriptionに明記 |
| 汎用すぎるdescription | 誤マッチ多発 | 具体的なキーワード |
| 単純なルール | skills不要 | rules/で十分 |

## _shared/ リソース配置

複数スキルで共有するリソースは `_shared/` に配置。

```
skills/
├── _shared/
│   ├── templates/     # 共通テンプレート
│   ├── guides/        # 共通ガイド
│   └── checklists/    # 共通チェックリスト
├── skill-a/
└── skill-b/
```

### 配置基準

- **2つ以上のスキルで使用**: `_shared/` に配置
- **1つのスキルのみ**: skill固有ディレクトリに配置
