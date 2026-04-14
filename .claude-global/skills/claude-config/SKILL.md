---
name: claude-config
description: Claude Code設定の診断・最適化と、ベストプラクティス記事からの自己改善。Use when「設定を最適化」「config」「Claude設定チェック」「ベストプラクティス反映」。
disable-model-invocation: true
allowed-tools: Read, Glob, Grep, Bash, Edit, Write, AskUserQuestion, WebFetch, Task
argument-hint: "[optimize | update <URL or text> | update auto]"
model: sonnet
effort: high
---

# claude-config

## 概要

Claude Code 設定を診断・最適化するスキル。3つのモードを持つ。

| モード | 用途 |
|--------|------|
| **診断** | `.claude/` 設定をスキャンし改善提案 |
| **自己改善** | ベストプラクティス記事からナレッジ（references/ + rules/）を更新 |
| **能動的探索** | 公式ドキュメントを自律探索し最新情報をナレッジに反映 |

知識の分担: `.claude-global/rules/`（パスマッチ自動読込）と `references/`（スキル実行時参照）。重複ゼロ原則。

## Step 0: モード判定

引数 `$ARGUMENTS` を確認:

- `update auto` → **能動的探索モード**（Step A へ）
- `update` で始まる（`auto` 以外）→ **自己改善モード**（Step U へ）
- それ以外（空含む）→ **診断モード**（Step 1 へ）

---

## 診断モード

### Step 1: 共通コンテキスト収集

Bash tool で scan-config.py を実行し、.claude/ 構成と project-context を取得する。

```
~/.dotfiles/.claude-global/skills/claude-config/scripts/scan-config.py
```

### Step 2: 3並列サブエージェント分析

3エージェントを単一メッセージの Task tool で foreground 並列起動（sonnet）する。各エージェントに Step 1 の共通コンテキスト出力を渡す。

各エージェントは自分でスクリプト実行 + references 読み込みを行い、フラグ条件に基づいて分析する。

| Agent | 目的関数 | 実行スクリプト | Read する references |
|-------|---------|--------------|---------------------|
| 1 | 設定した自動化戦略（hooks/permissions）が意図通りに機能しているか。手動承認の頻出パターンから自動化の穴を特定する | `scripts/scan-hooks.py`, `scripts/scan-metrics.py` | `references/hooks.md` |
| 2 | 推論的コンテキスト（CLAUDE.md/rules/memory）が Claude の行動を目的通りに方向づけているか。memory の内容が適切な永続化先に配置されているかも含む | `scripts/scan-claude-md.py` | `references/claude-md.md` |
| 3 | 各スキルが投資対効果に見合っているか。使用頻度と設計複雑度を照合し、未使用スキルの削除候補・頻出パターンのスキル化候補を特定する | `scripts/scan-skills.py` | `references/skills.md`, `references/skill-design-patterns.md` |

Agent 1 は `scan-hooks.py` に加えて `scan-metrics.py` も実行する。`scan-metrics.py` の出力（tool 別 PermissionRequest カウント・skill_usage 別スキル呼び出し回数）は permissions.allow / guard との相関分析および Agent 3 の投資対効果評価に使用する。

**各エージェントへの共通指示**:
1. スクリプトがある場合は実行して PASS/WARN 結果を取得
2. references を Read してフラグ条件・ベストプラクティスを把握
3. 実際の設定ファイルを Read して分析
4. WARN 項目を起点に詳細調査し、証拠付きで問題を報告

**出力形式**（全エージェント共通）:
| # | ファイル | 証拠 | 問題 | 改善案 |
証拠列にファイルパス+行番号 or 具体値を必須とする。

**Agent 2 の追加手順**（memory 昇華チェック）:
1. Step 1 の scan-config.py 出力から `encoded_cwd` を取得する
2. `~/.claude/projects/{encoded_cwd}/memory/` が存在する場合、MEMORY.md と各トピックファイルを Read する
3. 下記の昇華判定テーブルに従い、昇華・削除・維持の候補リストを出力する

| メモリタイプ | 昇華先 | 条件 |
|------------|-------|------|
| feedback（全体共通ルール） | CLAUDE.md | 全プロジェクトに適用、CLAUDE.mdに未記載 |
| feedback（パス固有ルール） | rules/ | 特定ファイル種別・ディレクトリに限定 |
| feedback（CLAUDE.mdと重複） | 削除 | すでにCLAUDE.mdに同内容がある |
| project（永続的な方針・制約） | CLAUDE.md or プロジェクトCLAUDE.md | 時限性がなく全般適用 |
| reference（外部リソース情報） | skills/references/ | 特定スキルに紐づく |
| 繰り返し参照されるワークフロー | skills/ | 手順として定型化できる |
| user型・一時的projectメモ | 維持 | 個人情報や一時的な作業状態 |

スクリプトパス: `~/.dotfiles/.claude-global/skills/claude-config/scripts/`
References パス: `~/.claude/skills/claude-config/references/`

### Step 3: 結果統合と改善提案

3エージェントの出力を統合し、証拠なき指摘を除外して優先度（Critical/High/Medium/Low）を割り当てる:

`| # | 優先度 | カテゴリ | 問題 | 改善案 |` の表形式で出力。Agent 2 のメモリ昇華候補も改善提案テーブルに含める。

### Step 4: 適用確認

AskUserQuestion で確認: `all`（全適用）/ `1,3`（番号指定）/ `none`（スキップ）

### Step 5: 編集適用

選択された番号の改善を Edit tool で適用する。

- `none` 選択 → レビューフェイズをスキップし完了サマリーへ
- 適用後に変更サマリーを出力
- 適用できない項目（手動確認が必要な場合）はその旨を明記

---

## 能動的探索モード

### Step A1: upstream-sources.md を読み込み

`Read ~/.claude/skills/claude-config/references/upstream-sources.md` で `last-auto-update` 日付・`last-known-version` と、固定URL・検索クエリ・監視対象ワークアラウンドを把握する。

### Step A2a: ドキュメント差分検出（スクリプト）

Bash tool で docs-diff.py を実行し、前回スナップショットからのドキュメント変更を検出する。

```
~/.dotfiles/.claude-global/skills/claude-config/scripts/docs-diff.py \
  --snapshot-dir ~/.dotfiles/.claude-global/skills/claude-config/snapshots/docs
```

`RESULT` 判定: 出力全体を保持し、Step A2b でサブエージェントに渡す。

### Step A2b: 調査サブエージェント起動

事前に `Read ~/.claude/skills/researcher/SKILL.md` で調査原則・引用ルールを取得し、Task tool (sonnet, allowed-tools: WebSearch, WebFetch, Write, Bash) で委譲する。

プロンプトに含める要素:
- researcher/SKILL.md から取得した調査原則・引用ルール
- 調査期間: `{last-auto-update}` から今日まで（バージョン起点: `{last-known-version}` 以降のリリース）
- **docs-diff.py の出力全文**（ドキュメント差分。サブエージェントはこの差分を分析対象とし、固定URLへの個別WebFetchは不要）
- upstream-sources.md の検索クエリ・監視対象ワークアラウンドを全文転記
- 調査観点: 新機能・破壊的変更・非推奨化 / settings/hooks/skills/CLAUDE.md 仕様変更 / Gotchas / ワークアラウンド解消確認
- バージョン調査: CHANGELOG.md から `{last-known-version}` 以降の全バージョンと変更内容を取得。調査時点の最新バージョンを記録する
- 成功基準: カテゴリ別整理・変更なし明記・出典URL付与
- 出力先: `.claude/research/YYYY-MM-DD-HHMMSS-claude-code-updates.md`（セクション: 調査概要 / カテゴリ別変更 / ワークアラウンド状況 / 参考資料）

### Step A3: 更新有無の判定

レポートファイルを Read し、内容を確認する。

- 全カテゴリ「変更なし」→「最新状態です」で終了
- 更新あり → Step A4 へ

### Step A4: 変更の反映

`Read ~/.claude/skills/claude-config/references/update-guide.md` でフローを把握し、各変更を処理する（カテゴリ特定 → 対象 Read → Edit 差分 → 整合性チェック）。

### Step A5: メタデータ・スナップショット更新

レポートから調査時点の最新バージョンを確認し、`upstream-sources.md` を以下の通り更新する:
- `last-auto-update` を本日日付（YYYY-MM-DD）に更新
- `last-known-version` を調査時点の最新バージョン番号（例: `2.1.98`）に更新

バージョンが不明な場合は `claude --version` を実行して現在値を取得する。

ドキュメントスナップショットを現在の内容で更新する:
```
~/.dotfiles/.claude-global/skills/claude-config/scripts/docs-diff.py \
  --snapshot-dir ~/.dotfiles/.claude-global/skills/claude-config/snapshots/docs \
  --update-snapshots
```

→ 共通レビューフェイズ（R1〜R3）へ

---

## 自己改善モード

### Step U1: 記事取得

- URL が渡された場合 → WebFetch で取得
- テキストが渡された場合 → そのまま使用

### Step U2〜U4: update-guide に従う

`Read ~/.claude/skills/claude-config/references/update-guide.md` で詳細手順を把握（Progressive Disclosure）。

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

Task tool (opus) で委譲。プロンプトに含める要素:
- モード: `${診断モード | 自己改善モード}`
- レビュー対象: changed-files.sh の出力パスリスト（各ファイルを Read して分析）
- レビュー観点: `references/review-criteria.md` の内容を展開（4観点）
- 出力形式: `issue_id: REVIEW-${cycle}-{sequential}` / `file` / `line` / `dimension: consistency|accuracy|format|utility` / `problem` / `suggestion`。issueなし→「指摘なし」

**issue判定（自身で実行）**:
- issueなし → 完了サマリーへ
- 前サイクルと同一issue（file+line一致） → `[RECURRING]` マーク、修正対象外
- issueあり → 修正へ

**修正（自身で実行）**: issueごとに Edit tool で適用する（設定ドキュメント修正は軽量なため自身で実行）。最大2サイクル到達時は残存issueを報告し完了サマリーへ。

### R3: レビュー結果ファイル書き出し

レビューを実行した場合、結果を `.claude/tmp/review/YYYY-MM-DD-HHMMSS-{topic}.md` に書き出す。

---

## 注意事項

- スクリプト実行: `~/.dotfiles/.claude-global/skills/claude-config/scripts/`（実ファイルパス。.dotfiles 配置前提）
- ドキュメント参照: `~/.claude/skills/claude-config/references/`（シンボリックリンク経由）
