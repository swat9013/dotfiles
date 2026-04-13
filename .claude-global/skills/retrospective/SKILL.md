---
name: retrospective
description: セッション会話からドメイン知識を抽出しmemoryに捕捉する。Use when「学びを保存」「振り返り」「retrospective」「ドメイン知識更新」。
disable-model-invocation: true
argument-hint: "[--since=Nd] [--limit=N]"
model: sonnet
effort: medium
---

# Retrospective

セッション会話からドメイン知識を抽出し、Auto Memory（memory files）に永続化するスキル。

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
セッション会話から再利用価値のある知見を抽出し、保存先を判断してください。

## 指示
1. Read tool で以下のファイルを読み込む（Bash や python3 は使わないこと）:
   - {手順1で取得したデータファイルパス}（セッションデータ）
   - データファイルの review_files に含まれる各ファイルパス（tmp/review/ の opusレビュー結果 md ファイル）があれば Read する
2. データファイルの sessions 配列を、以下の5つの検出レンズで走査する
3. existing_context と重複するものは除外する
4. 各知見を適切なメモリタイプに分類する（feedback/project/reference/破棄）

## 検出レンズ（5つの観点で会話を走査する）

### L1: 行動修正
ユーザーが Claude の行動・出力を否定・修正した箇所。
手がかり: 「そうじゃない」「〜しないで」「〜ではなく」+ Claude が方針転換。
→ type: feedback

### L2: Feedforward
ユーザーが今後の行動指針を与えた箇所。
手がかり: 「次からは」「今後は」「いつも」「〜のときは〜して」。
会話中の直接指示に限定（CLAUDE.md/rules との突き合わせは対象外）。
→ type: feedback

### L3: 発見された事実
調査・デバッグ中に判明した環境固有の事実。
手がかり: エラー→原因特定→解決のパターン、「〜が原因だった」「〜の仕様は〜」。
→ type: project / reference

### L4: 設計判断と根拠
選択肢があり、理由付きで1つを選んだ箇所。
手がかり: 「〜にした理由は」「〜より〜がいい」「トレードオフは〜」。
→ type: project

### L5: 探索の非効率
Claude が同一目的で複数回の探索（Glob/Grep/Read）を行い収束した箇所。
手がかり: 同じターゲットへの繰り返し検索、失敗→リトライ→成功パターン。
抽出すべきは効率的な到達パス（「X を探すときは Y のパスにある」）。
→ type: reference

## 出力形式
知見ごとに以下の形式で出力:

### 知見N: {1行タイトル}
- **lens**: L1〜L5 のいずれか
- **type**: feedback / project / reference / 破棄
  - feedback: ユーザーの行動修正・指摘パターン（全プロジェクト共通）
  - project: プロジェクト固有の方針・制約・設計決定
  - reference: 特定スキルに紐づく外部リソース・手順情報
  - ※ user タイプ（個人プロフィール）は retrospective の対象外
- **name**: <kebab-case、64文字以内>
- **description**: 1文の要約
- **内容**: 保存すべき内容（簡潔に）
- **根拠**: セッションからの引用（1-2文）

## 処理ルール
- 将来の別セッションで再利用価値がある知見のみ抽出（汎用ベストプラクティスは除外）
- 既存コンテキストと重複するものは除外
- 複数セッションで言及 → 信頼度UP
- 矛盾する知見は両方記載し矛盾を明記
- 知見が0件なら「知見なし」とだけ出力
- review_files に opusレビュー結果ファイルが含まれる場合、それらの指摘からも知見を抽出する（L3-L5 レンズが適用可能）
```

**エラー処理**: サブエージェントがエラーを返した場合、または出力が空の場合は1回リトライ。2回失敗したら「分析に失敗しました」と表示して終了。

### 3. Memory 捕捉

サブエージェントの出力（各知見の type / name / description / 内容）を受け取り、memory files に書き込む。

#### 既存 memory との重複確認

Glob tool で `~/.claude/projects/<encoded-cwd>/memory/*.md` を列挙する。

- encoded-cwd の算出: 現在の作業ディレクトリ（`$PWD`）の `/` を `-` に変換し先頭 `-` を保持
  - 例: `/Users/s-watanabe/.dotfiles` → `-Users-s-watanabe--dotfiles`
- 同じ `<type>_<slug>.md` のファイルが存在する場合は「既存と類似」として報告しスキップ（上書き禁止）

#### Auto Memory 形式ファイルの生成（破棄判定以外の知見ごと）

ファイルパス: `~/.claude/projects/<encoded-cwd>/memory/<type>_<name>.md`

Write tool で以下の形式で生成する:

```
---
name: <name>（サブエージェント出力の name フィールド）
description: <description>（サブエージェント出力の description フィールド）
type: <type>（feedback / project / reference）
---

<内容の本文>

**Why:** <根拠：セッションからの引用>

**How to apply:** <適用方法：具体的な場面・条件>
```

#### MEMORY.md 索引の更新

1. Read tool で `~/.claude/projects/<encoded-cwd>/memory/MEMORY.md` を読み込む
   - ファイルが存在しない場合は新規作成（内容は空）
2. 同名エントリ（`[<name>]`）が MEMORY.md に存在する場合は追記しない（冪等性）
3. 存在しない場合は末尾に以下を追記:
   `- [<name>](<filename>) — <description>`

### 4. 完了処理

#### 完了レポート

```
## 振り返り完了

### 更新内容
- memory files: X件の知見を新規作成
- MEMORY.md: X件のエントリを追記

### 除外項目
- N件（理由: 汎用的/一時的/既存重複/破棄判定）

### 次のステップ
蓄積されたmemoryをルール・スキルに昇格するには `/claude-config` を実行してください。
```

## 参照ファイル

| ファイル | 用途 |
|---------|------|
| `scripts/collect.py` | セッションデータ収集（6スクリプト統合） |

## 原則

- **ドメイン知識のみ**: 汎用的なベストプラクティスではなく、プロジェクト固有の知見
- **最小限の高シグナル情報**: 効果を最大化
- **横断パターン発見**: 全セッション一括分析で複数セッションにまたがるパターンを検出
