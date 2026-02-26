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

## ワークフロー

### 1. 引数解析

`$ARGUMENTS` を find-sessions.sh に渡す。

- 引数なし → 当日セッション（デフォルト）
- 例: `/retrospective --since=3d` → 3日前以降

### 2. セッション取得

```bash
~/.dotfiles/.claude-global/skills/retrospective/scripts/find-sessions.sh $ARGUMENTS
```

セッションファイルのパス一覧（改行区切り）を取得する。
30件を超える場合は `--limit=30` を付与して再取得し、ユーザーに通知する。

### 3. セッション準備

```bash
~/.dotfiles/.claude-global/skills/retrospective/scripts/prepare-sessions.sh {session_files}
```

出力（1行/セッション）:
- `CACHED <bytes> <cache_path>`: Stage 1 済み。cache_file を直接使用
- `NEW <bytes> <tmp_path>`: Stage 1 サブエージェントで抽出が必要
- 2000バイト未満のセッションは出力されない（知見抽出の価値が低い）

### 4. Two-Stage サブエージェント処理

**原則**: 親エージェントはセッション内容もテンプレートも読まない。ファイルパスだけを渡す。

**実行制約**:
- **foreground**（`run_in_background`未指定）で起動すること。結果は直接返却される
- **backgroundは使用禁止**（TaskOutput並列取得が衝突して全失敗するため）
- 1メッセージ内のTask呼び出しは**最大5個**。超過分は前バッチ完了後に次バッチ起動

| 設定 | Stage 1 | Stage 2 |
|------|---------|---------|
| subagent_type | general-purpose | general-purpose |
| model | haiku | sonnet |

#### Stage 1: Extract（Map）— NEW セッションのみ

§3 で `NEW` と判定されたセッションごとに1つのサブエージェントを起動する。
1つのサブエージェントに複数セッションを渡さない。

**事前準備**: 既存コンテキストを収集する（Stage 1 prompt に埋め込む）:
```bash
~/.dotfiles/.claude-global/skills/retrospective/scripts/collect-existing-context.sh
```

**Stage 1 prompt**:
```
プロジェクト固有のドメイン知識候補を抽出してください。

## 指示
1. 以下のファイルを Read tool で読み込む: {/tmp/retro_XXXXX.txt のパス}
2. プロジェクト固有の知見を箇条書きで抽出する
3. 汎用的なベストプラクティス・一時的な作業メモは除外する

## 既存コンテキスト（重複回避用）
{collect-existing-context.sh の出力}

## 出力形式
- **[カテゴリ]** 内容の1行要約 | 根拠: 引用（1-2文）| 保存先候補: CLAUDE.md / rules/ / skills/ / settings / 不明

カテゴリ: アーキテクチャ / 技術選択 / コーディング規約 / 落とし穴 / コマンド / 依存関係 /
データ構造 / 環境・設定 / 用語 / 境界条件 / パフォーマンス / 権限設定 / ワークフロー摩擦 / 設定改善

知見が0件なら「知見なし」とだけ出力。
```

**Stage 1 エラー処理**: 個別失敗は警告してスキップ（部分結果でも価値あり）。
全セッション「知見なし」の場合は「抽出可能な知見なし」と表示して終了。

**Stage 1 完了後**: 結果をキャッシュに保存する（§5参照）。

#### Stage 2: Classify（Reduce）— 1つのサブエージェント

Stage 1 の全結果（キャッシュ済み + 新規）を連結し、単一のサブエージェントに渡す。

**Stage 2 prompt**:
```
ドメイン知識候補を分類・構造化してください。

## 指示
1. 以下のファイルを Read tool で読み込む:
   - ~/.dotfiles/.claude-global/skills/retrospective/references/classification-criteria.md
   - ~/.dotfiles/.claude-global/skills/retrospective/templates/reflect-output.md
2. 下記の抽出結果を classification-criteria.md の基準で分類する
3. reflect-output.md の形式で出力する

## 抽出結果（全セッション統合）
{Stage 1 全結果を連結}

## 処理ルール
- 重複する知見はマージ（複数セッション言及 → 信頼度UP）
- 矛盾する知見は両方記載し矛盾を明記
- 既存コンテキストと重複するものは除外
```

**Stage 2 エラー処理**: 1回リトライ → 失敗時は Stage 1 結果を直接表示する。

### 5. 親エージェント処理

#### Stage 1 完了後

1. 各 NEW セッションの Stage 1 結果を Write tool でキャッシュに保存する:
   - パス: `.claude/retrospective-cache/{session-uuid}.txt`
   - 内容: Stage 1 サブエージェントの出力テキスト
2. `CACHED` セッションの cache_file を Read して Stage 1 結果に加える
3. 全 Stage 1 結果を連結して Stage 2 に渡す

#### Stage 2 完了後

Stage 2 の出力（構造化された分類結果）を受け取り、4層保存ロジックに進む。

Stage 2 失敗時は Stage 1 結果を直接表示し、ユーザーに手動分類を促す。

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
| `scripts/prepare-sessions.sh` | キャッシュチェック + 抽出 + ステータス判定 |
| `scripts/collect-existing-context.sh` | 既存コンテキスト収集（重複回避用） |

## 成功基準

1. 学びが再利用可能な形で適切な層に永続化されている
2. 既存内容と重複・矛盾がない
3. rules/skills/settings への書き込み前にユーザー確認が挟まる

## 原則

- **ドメイン知識のみ**: 汎用的なベストプラクティスではなく、プロジェクト固有の知見
- **最小限の高シグナル情報**: 効果を最大化
- **Progressive Disclosure**: 詳細は references/ と templates/ に分離
