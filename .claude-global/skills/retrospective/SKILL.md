---
name: retrospective
description: セッション会話からドメイン知識を抽出し4層（CLAUDE.md/rules/skills/settings）に永続化する。「学びを保存」「知見を保存」「ドメイン知識更新」「振り返り」「retrospective」「reflect」「セッションから学ぶ」「CLAUDE.md更新」「プロジェクト知識」「コンテキスト記録」「今の学びを残して」と依頼された時に使用。
disable-model-invocation: true
argument-hint: "[--since=Nd] [--limit=N]"
---

# Retrospective

セッション会話からドメイン知識を抽出し、4層の保存先に永続化するスキル。

## 対象

| ファイル | 内容 |
|----------|------|
| プロジェクトCLAUDE.md | プロジェクト固有のドメイン知識を蓄積 |
| .claude/rules/ | パス固有のガイドライン |
| .claude/skills/ | ワークフロー定義 |
| settings.local.json | 権限設定（allow/ask/deny） |

## モード選択

| モード | 用途 | トリガー |
|--------|------|----------|
| Reflect | セッション会話から自動抽出 | **デフォルト**（`/retrospective`） |
| Manual | 手動で知識を抽出・追記 | ユーザーが明示的に内容を指定 |

## Manual モード

親エージェントが直接実行する。サブエージェントは不要。

### 手順

1. ユーザーが指定した知見のカテゴリと保存先を判断
2. 既存 CLAUDE.md・rules/ との重複・矛盾を確認
3. 保存先ごとの確認フローに従って保存

| 保存先 | 確認フロー |
|--------|-----------|
| CLAUDE.md | 重複確認後、即時追記（既存セクションに追記 or 新規セクション作成） |
| rules/ | 差分提示 + AskUserQuestion で確認 |
| skills/ | 提案表示のみ（`/managing-skills` に委譲） |
| settings.local.json | JSON検証 + 差分提示 + 確認 |

## Reflect モード

セッション会話からドメイン知識を自動抽出する。

### 1. 引数解析

`$ARGUMENTS` を find-sessions.sh に渡す。

- 引数なし → 当日セッション（デフォルト）
- 例: `/retrospective --since=3d` → 3日前以降

### 2. セッション取得

```bash
~/.dotfiles/.claude-global/skills/retrospective/scripts/find-sessions.sh $ARGUMENTS
```

セッションファイルのパス一覧（改行区切り）を取得する。

### 3. メッセージ抽出

全セッションを一括で抽出・サイズフィルタする（Bash 1回）:

```bash
for f in {session_files}; do
  outfile="/tmp/retro_$(basename "$f" .jsonl).txt"
  ~/.dotfiles/.claude-global/skills/retrospective/scripts/extract-messages.sh "$f" --max-chars=30000 > "$outfile"
  size=$(wc -c < "$outfile")
  if [ "$size" -gt 2000 ]; then
    echo "$size $outfile"
  else
    rm -f "$outfile"
  fi
done
```

2000バイト未満のセッションはスキップ（内容が少なく知見抽出の価値が低い）。

### 4. サブエージェント委譲

セッション単位でサブエージェントを**foreground**並列起動する。1つのサブエージェントに複数セッションを渡さない（コンテキスト圧迫回避）。

**実行制約**:
- **foreground**（`run_in_background`未指定）で起動すること。結果は直接返却される
- **backgroundは使用禁止**（TaskOutput並列取得が衝突して全失敗するため）
- 1メッセージ内のTask呼び出しは**最大5個**。超過分は前バッチ完了後に次バッチ起動

| 設定 | 値 |
|------|-----|
| subagent_type | general-purpose |
| model | sonnet |

**prompt構造**:

親エージェントが事前に `classification-criteria.md` と `reflect-output.md` をReadし、その内容をpromptに直接埋め込む（サブエージェントのRead回数削減）。

```
ドメイン知識を抽出してください。

## セッションデータ
{extract-messages.sh の出力（/tmp/retro_*.txt の内容）}

## 既存コンテキスト
- CLAUDE.md: ./CLAUDE.md の概要（セクション見出しのみ）
- rules/: ./.claude/rules/ のファイル一覧

## 分類基準
{classification-criteria.md の内容を直接埋め込み}

## 出力フォーマット
{reflect-output.md の内容を直接埋め込み}
この形式に従って出力してください。
```

### 5. 親エージェント処理

サブエージェントの結果を受け取った後:

1. 複数セッションの結果をマージ + 重複排除
2. 既存 CLAUDE.md との重複チェック
3. 既存 .claude/rules/ との矛盾チェック

## 4層保存ロジック

**重要**: 保存先ごとに異なる確認フローを適用する。

### CLAUDE.md

- 既存セクションに追記、または新規セクション作成
- 重複確認後、確認なしで即時追記（Edit tool で直接追記）

### rules/

- 対象ファイル・glob パターンを含めた差分を AskUserQuestion で提示
- 承認後に既存ファイルに追記、または新規ファイル作成

### skills/

- 提案のみ表示（「以下のスキルの作成・更新を検討してください:」）
- 実際の作成は `/managing-skills` に委譲

### settings.local.json

1. 現在の設定を読み込み
2. 提案する変更を生成（allow/ask/deny の追加・変更）
3. `jq .` で JSON 構文検証
4. AskUserQuestion で現在値 → 提案値の差分を提示
5. 承認後に jq でマージ更新

## 追記後の行数チェック

```bash
wc -l ./CLAUDE.md
```

150行を超えた場合: 「CLAUDE.md が150行を超えました。`/context-optimizer` の実行を検討してください。」と表示。

## 完了レポート

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
| `templates/reflect-output.md` | 抽出結果テンプレート |
| `references/classification-criteria.md` | 14カテゴリ + 4層分類基準 |
| `scripts/find-sessions.sh` | セッション特定スクリプト |
| `scripts/extract-messages.sh` | メッセージ抽出スクリプト |

## 成功基準

1. 学びが再利用可能な形で適切な層に永続化されている
2. 既存内容と重複・矛盾がない
3. rules/skills/settings への書き込み前にユーザー確認が挟まる

## 原則

- **ドメイン知識のみ**: 汎用的なベストプラクティスではなく、プロジェクト固有の知見
- **最小限の高シグナル情報**: 効果を最大化
- **Progressive Disclosure**: 詳細は references/ と templates/ に分離
