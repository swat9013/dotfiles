---
name: managing-skills
description: Claude Codeスキルの作成・変更・更新に関する知識ガイド。frontmatter仕様、Progressive Disclosure、設計パターン、string substitutionsを含む。「スキルを作って」「スキルを変更」「スキルを更新」「新しいスキルを追加」と依頼された時に参照する。
user-invocable: false
---

# Skill Creator

## 概要

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
name: skill-name        # gerund形式推奨（processing-pdfs等）、64文字以内
description: ...        # 三人称で記述、1024文字以内、トリガーキーワードを含める
---
```

### オプションフィールド

| フィールド | 説明 | 例 |
|-----------|------|-----|
| `argument-hint` | オートコンプリート時のヒント | `[issue-number]` |
| `disable-model-invocation` | `true`でClaude自動呼び出し禁止 | `true` |
| `user-invocable` | `false`で/メニュー非表示 | `false` |
| `allowed-tools` | 許可確認なしで使用可能なツール | `Read, Grep, Glob` |
| `model` | 実行モデル指定 | `claude-sonnet-4-20250514` |
| `context` | `fork`でサブエージェント実行 | `fork` |
| `agent` | fork時のサブエージェントタイプ | `Explore` |
| `hooks` | スキルライフサイクルフック | (Hooks設定形式) |

### 呼び出し制御

| 設定 | ユーザー呼び出し | Claude呼び出し | 用途 |
|------|----------------|---------------|------|
| (デフォルト) | ○ | ○ | 汎用スキル |
| `disable-model-invocation: true` | ○ | ✕ | deploy, commit等の副作用あり |
| `user-invocable: false` | ✕ | ○ | 背景知識、コンテキスト |

## description設計

descriptionはClaude自動選択のトリガー。**三人称で具体的に書く**。

```yaml
# 悪い例（曖昧 or 視点が不適切）
description: ドキュメントを処理する
description: ユーザーのPDF処理を手伝う  # 二人称

# 良い例（三人称 + 具体的）
description: PDFからテキスト・表を抽出し、フォーム入力、文書結合を行う。PDF操作、フォーム記入、文書抽出と依頼された時に使用。
```

**重要**: 一人称・二人称は発見に問題を起こす。三人称で記述すること。

## 設計原則

1. **簡潔に**: SKILL.md本文は500行以下
2. **Claudeは賢い**: 既知の情報は書かない
3. **例 > 説明**: 冗長な説明より具体例
4. **Progressive Disclosure**: 詳細はreferencesに（1階層まで）
5. **参照深度制限**: SKILL.md → references/xxx.md（2階層以上は避ける）

詳細パターン: [patterns.md](references/patterns.md)

## String substitutions

スキル内容で使用可能な置換変数:

| 変数 | 説明 | 例 |
|------|------|-----|
| `$ARGUMENTS` | 全引数 | `/fix-issue 123` → `123` |
| `$ARGUMENTS[N]` | N番目の引数（0-indexed） | `$ARGUMENTS[0]` |
| `$N` | `$ARGUMENTS[N]`の短縮形 | `$0`, `$1` |
| `${CLAUDE_SESSION_ID}` | セッションID | ログ出力、ファイル名に使用 |

```yaml
---
name: fix-issue
---
Fix GitHub issue $ARGUMENTS following our coding standards.
# または
Migrate $0 from $1 to $2.
```

## 動的コンテキスト注入

`!｀command｀` 構文（｀は実際にはバッククォート）でシェルコマンド出力を事前注入:

```yaml
---
name: pr-summary
context: fork
agent: Explore
---
## PR context
- PR diff: !｀gh pr diff｀
- Changed files: !｀gh pr diff --name-only｀

Summarize this pull request...
```

※ 上記の ｀ は実際にはバッククォート(`)を使用

コマンドはスキル実行前に実行され、出力がプレースホルダを置換する。

## サブエージェント実行

`context: fork` でスキルを分離コンテキストで実行:

```yaml
---
name: deep-research
context: fork
agent: Explore  # Explore, Plan, general-purpose, またはカスタム
---
Research $ARGUMENTS thoroughly...
```

- スキル内容がサブエージェントのタスクになる
- `agent` でサブエージェントタイプを指定
- 会話履歴にアクセスしない独立実行

## 配置場所

| 場所 | パス | 優先度 |
|------|------|--------|
| Enterprise | 組織管理設定 | 1（最高） |
| Personal | `~/.claude/skills/` | 2 |
| Project | `.claude/skills/` | 3 |
| Plugin | `plugin-name:skill-name` | 4（名前空間分離） |

同名スキルは優先度の高い方が使用される。
skills と commands が同名の場合、**skillsが優先**。

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

## 成功基準

1. スキルディレクトリとSKILL.mdが作成されている
2. frontmatterにnameとdescription（トリガー含む）が記載されている
3. 実際にスキルを呼び出して期待する動作が得られる

## 完了チェックリスト

- [ ] スキルディレクトリを作成した
- [ ] SKILL.mdに必須フィールド（name, description）を記載した
- [ ] descriptionが三人称形式で、トリガーキーワードを含む
- [ ] 実際に呼び出して期待の動作をすることを確認した
