---
name: skill-creator
description: スキルの新規作成・更新ガイド。「スキルを作って」「新しいスキルを追加」「スキルを作成したい」と依頼された時に使用。Claude Codeの機能を拡張するスキルの設計・実装を支援。
---

# Skill Creator

Claude Codeの機能を拡張するスキルを作成・更新する。

## スキルの構成

```
skill-name/
├── SKILL.md           # 必須: frontmatter + 手順
├── references/        # 参照ドキュメント（必要時のみ読み込み）
├── scripts/           # 実行スクリプト（読み込まず実行）
└── assets/            # テンプレート・画像等
```

## 作成フロー

### 1. 要件確認

ユーザーに質問:
- 何を自動化/支援したいか
- どんな言葉で呼び出したいか（トリガー）
- 入力と出力は何か

### 2. スキル初期化

```bash
~/.dotfiles/.claude-global/skills/skill-creator/scripts/init_skill.sh <skill-name> <path>
```

### 3. SKILL.md編集

生成されたテンプレートを編集:
- frontmatterのdescriptionを具体的に
- 手順を簡潔に記述
- 詳細はreferencesに分離

### 4. 検証

実際にスキルを呼び出してテスト。

## frontmatter仕様

### 必須フィールド

```yaml
---
name: skill-name        # ハイフンケース、64文字以内
description: ...        # 1024文字以内、トリガーキーワードを含める
---
```

### オプションフィールド

| フィールド | 説明 | 例 |
|-----------|------|-----|
| allowed-tools | 使用可能ツール制限 | `Read, Grep, Glob` |
| model | 実行モデル指定 | `claude-sonnet-4-20250514` |
| context | fork指定でサブエージェント実行 | `fork` |
| user-invocable | /メニュー表示 | `false` |

## description設計

descriptionはClaude自動選択のトリガー。具体的に書く。

```yaml
# 悪い例
description: ドキュメントを処理する

# 良い例
description: PDFからテキスト・表を抽出、フォーム入力、文書結合。PDF操作、フォーム記入、文書抽出と依頼された時に使用。
```

## 設計原則

1. **簡潔に**: SKILL.md本文は500行以下
2. **Claudeは賢い**: 既知の情報は書かない
3. **例 > 説明**: 冗長な説明より具体例
4. **Progressive Disclosure**: 詳細はreferencesに

詳細パターン: [patterns.md](references/patterns.md)

## 配置場所

| 場所 | 用途 | 優先度 |
|------|------|--------|
| `~/.dotfiles/.claude-global/skills/` | 個人用（全プロジェクト） | 高 |
| `.claude/skills/` | プロジェクト固有 | 中 |
| plugin内 | 配布用 | 低 |

同名スキルは優先度の高い方が使用される。

## 具体例

scenario-to-chapterスキルの構造:

```
scenario-to-chapter/
├── SKILL.md              # 165行、手順と変換例
└── references/
    ├── manga-technique-guide.md
    └── chapter.schema.json
```

SKILL.mdはワークフローと具体例に集中。詳細な技法はreferencesに分離。
