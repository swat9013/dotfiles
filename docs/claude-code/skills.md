# Skills 設計ガイド

## 概要

Claudeが実行するワークフローや手順を定義した再利用可能なコンポーネント。明示的な呼び出し（`/skill-name`）またはキーワードマッチで起動する。

## 設計原則

1. **簡潔さ**: SKILL.md本文は500行以下
2. **Progressive Disclosure**: 詳細は`references/`に分離
3. **具体例 > 説明**: Claudeは賢いので冗長な説明は不要
4. **明確なトリガー**: `description`にキーワードを含める

## ディレクトリ構成

```
skill-name/
├── SKILL.md           # 必須: 手順・ガイドライン
├── references/        # 参照ドキュメント（必要時のみ読み込み）
├── scripts/           # 実行スクリプト（読み込まず実行）
└── assets/            # テンプレート・画像等
```

## 配置場所と優先度

| 場所 | パス | 優先度 |
|------|------|--------|
| Enterprise | 組織管理設定 | 1（最高） |
| Personal | `~/.claude/skills/` | 2 |
| Project | `.claude/skills/` | 3 |
| Plugin | `plugin-name:skill-name` | 4（名前空間分離） |

- 同名スキルは優先度の高い方が使用される
- **skills と commands が同名の場合、skillsが優先**
- `.claude/commands/` は引き続き動作するが、skillsへの移行を推奨

## skills vs agents vs rules

| 観点 | skills/ | agents/ | rules/ |
|------|---------|---------|--------|
| 用途 | ワークフロー・手順定義 | カスタムサブエージェント | パス固有のルール |
| 実行形態 | 対話型・相談型 | コンテキスト分離実行 | 自動適用 |
| トリガー | Claude自動選択 or `/skill-name` | Task tool経由 | ディレクトリアクセス時 |

### skills vs agents 判断基準

| 判断基準 | skills | agents |
|---------|--------|--------|
| コンテキスト | 共有（会話継続） | 分離（独立実行） |
| 適用場面 | 手順定義、ガイド | 並列実行、試行錯誤、専門役割 |

## frontmatter仕様

| フィールド | 必須 | 説明 | 制約 |
|-----------|------|------|------|
| name | No | スキル識別子（省略時はディレクトリ名） | 小文字・数字・ハイフン、64文字以内 |
| description | 推奨 | 説明とトリガー（省略時はマークダウン最初の段落） | Claudeの自動選択に使用 |
| argument-hint | No | オートコンプリート時のヒント | `[issue-number]`等 |
| disable-model-invocation | No | Claude自動呼び出しを禁止 | `true`でユーザー専用 |
| user-invocable | No | /メニュー表示 | `false`で非表示 |
| allowed-tools | No | 許可確認なしで使用可能なツール | カンマ区切り |
| model | No | モデル指定 | claude-sonnet-4等 |
| context | No | 実行コンテキスト | `fork` でサブエージェント |
| agent | No | fork時のサブエージェントタイプ | `Explore`, `Plan`, `general-purpose` |
| hooks | No | スキルライフサイクルフック | Hooks設定形式 |

### 呼び出し制御

| 設定 | ユーザー | Claude | 用途 |
|------|---------|--------|------|
| (デフォルト) | ○ | ○ | 汎用スキル |
| `disable-model-invocation: true` | ○ | ✕ | deploy, commit等の副作用あり |
| `user-invocable: false` | ✕ | ○ | 背景知識、コンテキスト |

### frontmatter例

```yaml
---
name: my-skill
description: |
  スキルの説明。トリガーキーワードを含める。
  「設計して」「plan.md作成」と依頼された時に使用。
argument-hint: [target-file]

# ツール制限（オプション）
allowed-tools: Read, Glob, Grep, Task, WebSearch

# Claude自動呼び出しを禁止（オプション）
disable-model-invocation: true
---
```

## 命名規則

| 形式 | 推奨度 | 例 |
|------|--------|-----|
| gerund形式 | ◎ 推奨 | `processing-pdfs`, `analyzing-data` |
| 名詞句 | ○ 許容 | `pdf-processing`, `data-analysis` |
| 動詞形 | ○ 許容 | `process-pdfs`, `analyze-data` |
| 曖昧な名前 | ✕ 避ける | `helper`, `utils`, `tools` |

## description設計

**必須要素:**
- スキルの目的（1行目）
- トリガーキーワード（「〜と依頼されたら」形式）

**記述ルール:**
- **三人称で記述する**（一人称・二人称は発見に問題を起こす）
- システムプロンプトに注入されるため、視点の一貫性が重要

```yaml
# 悪い例（曖昧 or 視点が不適切）
description: ドキュメントを処理する
description: ユーザーのPDF処理を手伝う  # 二人称

# 良い例（三人称 + 具体的）
description: |
  システム設計・アーキテクチャ評価を担当する専門エージェント。
  「設計して」「アーキテクチャを考えて」「plan.md作成」と依頼された時に使用。
```

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

`$ARGUMENTS`がスキル内容に含まれない場合、引数は末尾に`ARGUMENTS: <value>`として追加される。

## 動的コンテキスト注入

`!`command`` 構文でシェルコマンド出力を事前注入:

```yaml
---
name: pr-summary
context: fork
agent: Explore
---
## PR context
- PR diff: !`gh pr diff`
- Changed files: !`gh pr diff --name-only`

Summarize this pull request...
```

- コマンドはスキル実行前に実行され、出力がプレースホルダを置換
- Claudeはコマンドではなく、実行結果のみを受け取る

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

複雑なワークフローにはチェックリストを提供:

進捗チェックリスト:
- [ ] Phase 1: 調査完了
- [ ] Phase 2: 実行完了
- [ ] Phase 3: 検証完了

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

### 参照先の明示

詳細をreferences/に分離した場合、参照先パスを明記:
- 「詳細は `references/xxx.md` を参照」
- リンク切れを防ぐため相対パスを使用

### 参照深度の制限

参照は**1階層まで**に制限する。深いネストはClaudeの部分読み込みで情報欠落を起こす。

```
# ❌ 悪い例（2階層）
SKILL.md → advanced.md → details.md

# ✅ 良い例（すべて1階層）
SKILL.md → advanced.md
SKILL.md → reference.md
SKILL.md → examples.md
```

### 長いリファレンスの構成

100行を超えるリファレンスファイルには先頭に目次を配置:
- Claudeの部分読み込み（`head -100`等）に対応
- 全体構造を俯瞰可能に

```markdown
# API Reference

## 目次
- 認証とセットアップ
- コアメソッド（CRUD）
- 高度な機能
- エラーハンドリング
- コード例

## 認証とセットアップ
...
```

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

## サブエージェント呼び出し

skill内でTask toolを使ってサブエージェントを動的に起動する。

### 基本パターン

```markdown
## Step 2: 並列レビュー

以下のサブエージェントを**単一メッセージで並列起動**:

### Agent 1: セキュリティチェック（Opus）
Task tool:
- model: opus
- prompt: |
    あなたはセキュリティ専門のレビュアーです。
    diffのみに集中。フラグすべき: SQLインジェクション、XSS

### Agent 2: ロジックチェック（Sonnet）
Task tool:
- model: sonnet
- prompt: |
    あなたはロジック専門のレビュアーです。
    ...
```

### モデル選択基準

| タスク種別 | モデル | 例 |
|-----------|--------|-----|
| 軽量チェック | Haiku | 前提条件チェック、ファイル検索 |
| 中程度の分析 | Sonnet | サマリー生成、コンプライアンスチェック |
| 複雑な判断 | Opus | バグ検出、セキュリティ分析 |

### 並列実行のガイドライン

- **単一メッセージ**で複数Task tool呼び出し
- 同時実行数は5-7程度を目安
- 依存関係がなければ並列、あれば順次

## フィードバックループ

品質向上には「実行→検証→修正→繰り返し」パターンが効果的:

```markdown
## ドキュメント編集プロセス

1. 編集を実行
2. **即座に検証**: `python scripts/validate.py`
3. 検証失敗時:
   - エラーメッセージを確認
   - 問題を修正
   - 再度検証
4. **検証パスまで繰り返し**
5. 出力を生成
```

検証ループにより早期にエラーを検出できる。

## コンテンツガイドライン

### 用語の一貫性

同じ概念には同じ用語を使用:
- ✅ 一貫: "extract"のみ使用
- ❌ 混在: "extract", "pull", "get", "retrieve"を混用

### 時間依存情報の回避

日付や期限に依存する情報は避ける:

```markdown
# ❌ 悪い例
2025年8月以降は新APIを使用してください。

# ✅ 良い例（旧パターンセクションで対応）
## 現在の方法
v2 APIエンドポイントを使用: `api.example.com/v2/`

<details>
<summary>旧パターン（v1 API、2025-08廃止）</summary>
v1 APIは `api.example.com/v1/` を使用していた。
</details>
```

### パス表記

常にスラッシュ（/）を使用:
- ✅ `scripts/helper.py`
- ❌ `scripts\helper.py`

## アンチパターン

| パターン | 問題点 | 対策 |
|---------|--------|------|
| 500行超のSKILL.md | 初期読み込みコスト大 | references/に分離 |
| トリガーキーワードなし | 自動選択されない | descriptionに明記 |
| 汎用すぎるdescription | 誤マッチ多発 | 具体的なキーワード |
| 単純なルール | skills不要 | rules/で十分 |
| 深い参照ネスト | 情報欠落 | 1階層までに制限 |
| 選択肢の提示過多 | 混乱を招く | デフォルトを示す |

## トラブルシューティング

### スキルが発動しない

1. descriptionにユーザーが使う言葉を含めているか確認
2. `What skills are available?` でスキル一覧を確認
3. 直接 `/skill-name` で呼び出してテスト

### スキルが頻繁に誤発動する

1. descriptionをより具体的に
2. `disable-model-invocation: true` で手動呼び出しのみに制限

### スキルが読み込まれない

- スキルdescriptionは**15,000文字のbudget制限**あり
- `/context` で除外されたスキルを確認
- `SLASH_COMMAND_TOOL_CHAR_BUDGET`環境変数で上限を調整可能

## 参考資料

- [Extend Claude with skills](https://code.claude.com/docs/en/skills) - 公式ドキュメント
- [A complete guide to building skills for Claude](https://claude.com/blog/complete-guide-to-building-skills-for-claude) - 公式ガイド
