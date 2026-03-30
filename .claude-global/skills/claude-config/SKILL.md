---
name: claude-config
description: Claude Code設定の診断・最適化と、ベストプラクティス記事からの自己改善。Use when「設定を最適化」「config」「Claude設定チェック」「ベストプラクティス反映」。
disable-model-invocation: true
allowed-tools: Read, Glob, Grep, Bash, Edit, Write, AskUserQuestion, WebFetch, Task
argument-hint: "[optimize | update <URL or text> | update auto]"
---

# claude-config

## 概要

Claude Code 設定を診断・最適化するスキル。3つのモードを持つ。

- **診断モード**: プロジェクトの `.claude/` 設定をスキャンし、ベストプラクティスに照らして改善提案
- **自己改善モード**: 新しいベストプラクティス記事を渡すと、スキルのナレッジ（references/ と rules/）を更新
- **能動的探索モード**: 公式ドキュメント・記事を自律的に探索し、最新ベストプラクティスをナレッジに反映

## 知識の分担

| 格納先 | 担当カテゴリ | 読み込みタイミング |
|--------|------------|-----------------|
| `.claude-global/rules/` | settings, rules のベストプラクティス | パスマッチ時に自動読込 |
| `references/` | hooks, claude-md, skills（診断モード）、subagent, task-management, context-architecture（自己改善モード / 能動的探索モード） | スキル実行時に参照 |

重複ゼロ原則: 両者に同じ情報を書かない。

## Step 0: モード判定

引数 `$ARGUMENTS` を確認:

- `update auto` → **能動的探索モード**（Step A へ）
- `update` で始まる（`auto` 以外）→ **自己改善モード**（Step U へ）
- それ以外（空含む）→ **診断モード**（Step 1 へ）

---

## 診断モード

### Step 1: スキャンスクリプト実行

Bash tool でスクリプトを実行し、構造化テキスト出力を取得する。

```
~/.dotfiles/.claude-global/skills/claude-config/scripts/scan-config.sh
```

出力が空や異常な場合は、スクリプトの存在・実行権限を確認する。

### Step 2: 3並列サブエージェント分析

各エージェントの担当 references を Read してプロンプトに展開し、3エージェントを単一メッセージの Task tool で foreground 並列起動（sonnet）する。エージェントは実際の設定ファイル（settings.json, CLAUDE.md 等）を自分で Read して分析する。

- Agent 1 → `~/.claude/skills/claude-config/references/hooks.md` を Read
- Agent 2 → `~/.claude/skills/claude-config/references/claude-md.md` を Read
- Agent 3 → `~/.claude/skills/claude-config/references/skills.md` を Read

各エージェントには、ロール・フォーカス範囲・フラグ条件・出力形式・Step 1 のスキャン出力・上記 references 内容・**scan-config.sh が出力する構造ヒントセクション（project-context, hooks-scripts, rules-paths-check）の参照指示** を渡す。

**Agent 1 プロンプト仕様**

```
## ロール
Claude Code の settings.json と hooks 設定の検証専門家。

## フォーカス範囲
調査対象（すべて自分で Read して分析する）:
- .claude/settings.json, settings.local.json
- ~/.claude/settings.json（グローバル）
- hooks から参照されるスクリプト（実装パターン検証）
- permissions.allow と deny の矛盾検出
- hooks の二層防御パターン（hooks + permissions.deny）の完全性
- settings.json に $schema が設定されているか
除外: CLAUDE.md 内容、rules/ 内容、skills/ 内容

## フラグ条件
報告すべき:
- hooks スクリプトが存在しないパスを参照
- permissions.allow と deny の矛盾
- Stop フックに stop_hook_active チェックなし
- 過度に広い permissions.allow（Bash(*) 等）
- 機密ファイルが deny に含まれていない
報告不要:
- hooks が未設定（小規模プロジェクトでは妥当）
- スタイルの好み
- プロジェクト固有でない一般ベストプラクティス

## 出力形式
| # | ファイル | 証拠 | 問題 | 改善案 |
証拠列にファイルパス+行番号 or 具体値を必須とする。
```

**Agent 2 プロンプト仕様**

```
## ロール
CLAUDE.md と rules/ の内容品質・相互整合性の検証専門家。

## フォーカス範囲
調査対象（すべて自分で Read して分析する）:
- CLAUDE.md
- .claude/rules/ 全ファイル
- CLAUDE.md ↔ rules/ 間の矛盾・重複検出（DRY原則）
- CLAUDE.md に rules/ や skills/ に移動すべき内容がないか
- CLAUDE.md 行数（150行推奨上限）
- rules/ の paths 形式（YAML配列形式はバグ、カンマ区切り必須）
- rules/ 各ファイル行数（200行以下）
- コンテキスト層の配置効率（高頻度情報が上位層にあるか）
除外: settings.json 内容、hooks 実装、skills/ 内容

## フラグ条件
報告すべき:
- CLAUDE.md と rules/ の矛盾（相反する指示）
- CLAUDE.md 150行超過
- rules/ の paths が YAML配列形式
- CLAUDE.md にパス固有のルールが直書き
- CLAUDE.md と rules/ に同一情報の重複
報告不要:
- 用語が完全一致しないが意味は同じ場合
- 行数が推奨内だが最適でない場合

## 出力形式
| # | ファイル | 証拠 | 問題 | 改善案 |
証拠列にファイルパス+行番号 or 具体値を必須とする。
```

**Agent 3 プロンプト仕様**

```
## ロール
skills/ 設計と .claude/ 構成の網羅性検証の専門家。

## フォーカス範囲
調査対象（すべて自分で Read して分析する）:
- .claude/skills/ と .claude-global/skills/ の各 SKILL.md
- description 形式検証（130文字以下、三人称動詞、Use when + トリガー3-5個）
- name と description の両方が存在するか
- SKILL.md 行数（500行以下）
- .claude/commands/ が存在する場合、skills/ への移行推奨
- .claude/agents/ が存在する場合、CLAUDE.md の方針との整合確認
- CLAUDE.md に必須コンテンツ（プロジェクト概要、主要コマンド）があるか
除外: settings.json の permissions/hooks 詳細、rules/ の内容品質

## フラグ条件
報告すべき:
- CLAUDE.md が存在しない
- CLAUDE.md にプロジェクト概要がない
- skills の SKILL.md に name/description 欠落
- description が 130文字超過 or トリガーなし
- .claude/commands/ が未移行
報告不要:
- 一般ベストプラクティスだがプロジェクト不要なもの
- 「あると便利」レベルの追加提案

## 出力形式
| # | ファイル | 証拠 | 問題 | 改善案 |
証拠列にファイルパス+行番号 or 具体値を必須とする。
```

### Step 3: 結果統合と改善提案

3エージェントの出力を統合し、証拠なき指摘を除外して優先度を割り当てる:

```
| # | 優先度 | カテゴリ | 問題 | 改善案 |
|---|--------|---------|------|--------|
| 1 | Critical | hooks | ... | ... |
| 2 | High    | skills | ... | ... |
```

優先度の定義:

- **Critical**: セキュリティリスク・動作不全
- **High**: ベストプラクティス違反で品質低下
- **Medium**: 改善余地あり
- **Low**: スタイル・任意の最適化

### Step 4: 適用確認

AskUserQuestion で確認:

```
改善を適用しますか？
  all    - すべて適用
  1,3    - 番号指定（カンマ区切り）
  none   - 適用しない
```

### Step 5: 編集適用

選択された番号の改善を Edit tool で適用する。

- `none` 選択 → レビューフェイズをスキップし完了サマリーへ
- 適用後に変更サマリーを出力
- 適用できない項目（手動確認が必要な場合）はその旨を明記

---

## 能動的探索モード

### Step A1: upstream-sources.md を読み込み

```
Read ~/.claude/skills/claude-config/references/upstream-sources.md
```

`last-auto-update` 日付と、固定URL・検索クエリ・監視対象ワークアラウンドを把握する。

### Step A2: 調査サブエージェント起動

Task tool (sonnet, allowed-tools: WebSearch, WebFetch, Write, Bash) で調査を委譲する。Skill tool 連鎖不可のため、researcher の調査原則・引用ルールをプロンプトに転記して再現する。

```
Task tool (sonnet, allowed-tools: WebSearch, WebFetch, Write, Bash):
  prompt: |
    Claude Code の最新変更とベストプラクティスを調査する。

    ## 調査原則（researcher準拠）
    - 公式ドキュメントを優先（二次情報より一次情報）
    - 最新情報を確認（バージョン、非推奨化、破壊的変更）
    - 「知っている」と思っても、最新情報はツールで確認する

    ## 引用ルール
    - すべての事実情報に出典を付与（フォーマット: `[出典名](URL) (YYYY-MM)`）
    - 複数の情報源を優先（単一ソース依存を避ける）
    - 矛盾する情報がある場合は両方を記載し、差異を説明

    ## 調査期間
    {last-auto-update} から {今日の日付} まで

    ## 必須チェックソース
    {upstream-sources.md の固定URL・検索クエリを全文転記}

    ## 調査観点
    - 新機能・破壊的変更・非推奨化
    - settings/hooks/skills/subagent/CLAUDE.md の仕様変更
    - 新しいベストプラクティス・Gotchas
    - ベストプラクティス記事の発見と知見抽出（検索クエリ結果から質の高い記事を選別し、設定改善に活かせるパターン・テクニックを抽出する。公式情報と矛盾する場合は公式を優先）
    - 以下のワークアラウンドに関連するバグ修正・機能実装の有無
      {upstream-sources.md の監視対象ワークアラウンドを全文転記}

    ## 成功基準
    - 各ソースの最終更新日以降の変更をカテゴリ別に整理
    - 変更なしの場合も「変更なし」と明記
    - すべての事実情報に出典URLと日付を付与

    ## 出力
    1. `~/.dotfiles/.claude-global/skills/scripts/claude-output-init.sh research` を実行
    2. 調査結果を `.claude/research/YYYY-MM-DD-HHMMSS-claude-code-updates.md` に Write
       - セクション構成: 調査概要 / カテゴリ別変更 / ワークアラウンド状況 / 参考資料
```

### Step A3: 更新有無の判定

レポートファイルを Read し、内容を確認する。

- 全カテゴリ「変更なし」→「最新状態です」で終了
- 更新あり → Step A4 へ

### Step A4: 変更の反映

```
Read ~/.claude/skills/claude-config/references/update-guide.md
```

レポートの各変更を update-guide.md フローで処理する（カテゴリ特定 → 対象 Read → Edit 差分 → 整合性チェック）。

### Step A5: last-auto-update を更新

```
Edit ~/.dotfiles/.claude-global/skills/claude-config/references/upstream-sources.md
  last-auto-update を本日日付（YYYY-MM-DD）に更新
```

→ 共通レビューフェイズ（R1〜R3）へ

---

## 自己改善モード

### Step U1: 記事取得

- URL が渡された場合 → WebFetch で取得
- テキストが渡された場合 → そのまま使用

### Step U2〜U4: update-guide に従う

```
Read ~/.claude/skills/claude-config/references/update-guide.md
```

詳細手順は update-guide.md に委譲（Progressive Disclosure）。

---

## 共通: レビューフェイズ（最大2サイクル）

診断モード Step 5 / 自己改善モード U4 / 能動的探索モード A5 の変更適用後に実行する。

### R1: 対象ファイル特定

```bash
~/.dotfiles/.claude-global/skills/scripts/changed-files.sh
```

`RESULT` 判定:
- `NO_CHANGES` → レビュースキップ、完了サマリーへ
- それ以外 → R2 へ

### R2: レビュー・修正サイクル

```
レビュー(opus) → issue判定 → 修正 → 次サイクル or 完了
```

**レビュー（サブエージェント委譲・opus）**:

モードに応じてプロンプトのコンテキスト部分を切り替える。

```
Task tool

subagent_type: general-purpose
model: opus
prompt: |
  あなたは Claude Code 設定ドキュメントのレビュー専門家です。

  ## モード
  ${診断モード | 自己改善モード}

  ## レビュー対象ファイル
  ${changed-files.sh の出力パスリスト}

  ## レビュー観点（4観点）
  ${references/review-criteria.md の内容を展開}

  ## 出力形式

  各issueを以下の形式で出力:

  issue_id: REVIEW-${cycle}-{sequential}
  file: パス
  line: 行番号
  dimension: consistency | accuracy | format | utility
  problem: 問題の1行要約
  suggestion: 修正案

  issueがない場合は「指摘なし」とだけ出力。
```

**issue判定（自身で実行）**:
- issueなし → 完了サマリーへ
- 前サイクルと同一issue（file+line一致） → `[RECURRING]` マーク、修正対象外
- issueあり → 修正へ

**修正（自身で実行）**:

issueごとに Edit tool で修正を適用する。設定ドキュメントの修正はコード修正より軽量なため、サブエージェント委譲せず自身で実行する。

**最大2サイクル到達**: 残存issueを報告し完了サマリーへ。

### R3: レビュー結果ファイル書き出し

レビューを実行した場合、結果を `.claude/review/YYYY-MM-DD-HHMMSS-{topic}.md` に書き出す。

```bash
~/.dotfiles/.claude-global/skills/scripts/claude-output-init.sh review
```

---

## 注意事項

- スクリプト実行: `~/.dotfiles/.claude-global/skills/claude-config/scripts/`（実ファイルパス。.dotfiles 配置前提）
- ドキュメント参照: `~/.claude/skills/claude-config/references/`（シンボリックリンク経由）
