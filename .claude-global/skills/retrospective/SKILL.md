---
name: retrospective
description: セッション会話からドメイン知識を抽出し4層に永続化。Use when「学びを保存」「振り返り」「retrospective」「ドメイン知識更新」。
disable-model-invocation: true
argument-hint: "[--since=Nd] [--limit=N]"
---

# Retrospective

セッション会話からドメイン知識を抽出し、4層の保存先に永続化するスキル。

## ワークフロー

### 1. データ収集

collect.py でセッションデータを収集する。

```bash
~/.dotfiles/.claude-global/skills/retrospective/scripts/collect.py $ARGUMENTS
```

- 引数なし → 当日セッション
- `--since=7d` → 過去7日分
- `--since=2026-03-01` → 日付指定
- `--limit=10` → 件数制限
- 2000バイト未満のセッションは自動除外

stdout にサマリー（total, skipped, セッション一覧）と `file=<パス>` が表示される。
詳細データは stdout の `file=` 行から取得したパスに書き出される。

**パス取得**: stdout から `file=` で始まる行を抽出し、以降のパスをデータファイルパスとして使用する。

**結果確認**: stdout の `total=0` なら「対象セッションなし」と表示して終了。

### 2. サブエージェント分析

手順1で取得したデータファイルパスを **1つの Sonnet サブエージェント**で一括分析する。

**実行制約**:
- **foreground**（`run_in_background` 未指定）で起動
- subagent_type: `general-purpose`、model: `sonnet`

**サブエージェント prompt**:

```
セッション会話からプロジェクト固有のドメイン知識を抽出し、保存先を判断してください。

## 指示
1. Read tool で以下の2ファイルを読み込む（Bash や python3 は使わないこと）:
   - {手順1で取得したデータファイルパス}（セッションデータ）
   - ~/.dotfiles/.claude-global/skills/retrospective/references/layer-criteria.md（判断基準）
2. retro-data.json の sessions 配列から知見を抽出する
3. existing_context と重複するものは除外する
4. layer-criteria.md の基準で各知見の保存先を判断する

## 出力形式
知見ごとに以下の形式で出力:

### 知見N: {1行タイトル}
- **保存先**: CLAUDE.md / rules/ / skills/ / settings / 破棄
- **対象パス** (rules/の場合): globパターン
- **内容**: 保存すべき内容（簡潔に）
- **根拠**: セッションからの引用（1-2文）

## 処理ルール
- プロジェクト固有の知見のみ抽出（汎用ベストプラクティスは除外）
- 既存コンテキストと重複するものは除外
- 複数セッションで言及 → 信頼度UP
- 矛盾する知見は両方記載し矛盾を明記
- 知見が0件なら「知見なし」とだけ出力
```

**エラー処理**: サブエージェントがエラーを返した場合、または出力が空の場合は1回リトライ。2回失敗したら「分析に失敗しました」と表示して終了。

### 3. 4層保存ロジック

サブエージェントの出力を受け取り、保存先ごとに処理する。

#### CLAUDE.md
- 既存セクションに追記、または新規セクション作成
- 重複確認後、確認なしで即時追記（Edit tool で直接追記）

#### rules/
- 対象ファイル・glob パターンを含めた差分を AskUserQuestion で提示
- 承認後に既存ファイルに追記、または新規ファイル作成

#### skills/
- 提案のみ表示（「以下のスキルの作成・更新を検討してください:」）
- 実際の作成は `/managing-skills` に委譲

#### settings.local.json
1. 現在の設定を読み込み
2. 提案する変更を生成（allow/ask/deny の追加・変更）
3. `jq .` で JSON 構文検証
4. AskUserQuestion で現在値 → 提案値の差分を提示
5. 承認後に jq でマージ更新

### 4. 完了処理

#### 行数チェック

```bash
wc -l ./CLAUDE.md
```

150行超: 「CLAUDE.md が150行を超えました。`/context-optimizer` の実行を検討してください。」

#### 完了レポート

```
## 振り返り完了

### 更新内容
- CLAUDE.md: X件の知見を追記
- rules/: Y件の提案（Z件承認済み）
- skills/: W件の提案（実行は /managing-skills で）
- settings.local.json: V件の権限変更（U件承認済み）

### 除外項目
- N件（理由: 汎用的/一時的/既存重複）
```

## 参照ファイル

| ファイル | 用途 |
|---------|------|
| `scripts/collect.py` | セッションデータ収集（6スクリプト統合） |
| `references/layer-criteria.md` | 4層保存先判断基準 |

## 原則

- **ドメイン知識のみ**: 汎用的なベストプラクティスではなく、プロジェクト固有の知見
- **最小限の高シグナル情報**: 効果を最大化
- **横断パターン発見**: 全セッション一括分析で複数セッションにまたがるパターンを検出
