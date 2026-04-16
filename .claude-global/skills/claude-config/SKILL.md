---
name: claude-config
description: Claude Code設定の診断・最適化と、ベストプラクティス記事からの自己改善。Use when「設定を最適化」「config」「Claude設定チェック」「ベストプラクティス反映」。
disable-model-invocation: true
allowed-tools: Read, Glob, Grep, Bash, Edit, Write, AskUserQuestion, WebFetch, Task
argument-hint: "[optimize | update <URL or text> | update auto]"
model: opus
effort: high
---

# claude-config

## 概要

Claude Code 設定を診断・最適化するスキル。3つのモードを持つ。

| モード | 用途 | 固有フェーズ | 共通フェーズ |
|--------|------|------------|------------|
| **診断** | `.claude/` 設定をスキャンし改善提案 | scan → 並列分析 → 統合 → 適用 | レビュー(R1-R3) |
| **自己改善** | 記事からナレッジ更新 | 記事取得 → update-guide 反映 | レビュー(R1-R3) |
| **能動的探索** | 公式ドキュメント自律探索 | docs-diff → 調査 → update-guide 反映 | レビュー(R1-R3) |

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

3エージェントを単一メッセージの Task tool で foreground 並列起動（sonnet）する。

各エージェントのプロンプトは `references/diagnostic-agent-prompt.md` のテンプレートに従い、以下のテーブルからエージェント固有の値を埋める。Step 1 の scan-config.py 出力全文をコンテキストとして渡す。

各 Agent の主目的関数は診断対象ドメインに一致する（**診断対象ドメイン一致原則**）。Agent 1/2 は settings.json / CLAUDE.md を診断するため `#behaviour-harness` 主、Agent 3 は skills を診断するため `#maintainability-harness` 主となる。本体（Architecture Fitness主）との切り替えは設計判断であり、親子目的関数非対称配置の一形態。

| Agent | 目的関数 | 実行スクリプト | Read する references | 追加指示 |
|-------|---------|--------------|---------------------|---------|
| 1 | `docs/harness-engineering-domain-model.md` の [#behaviour-harness] の三項組を Read し目的関数とする。加えて [#maintainability-harness] の境界条件のみ Read し侵食禁止制約とする。分析・指摘は三項組への因果で記述する。 | `scripts/scan-hooks.py`, `scripts/scan-metrics.py` | `references/hooks.md`, `references/settings.md` | - |
| 2 | `docs/harness-engineering-domain-model.md` の [#behaviour-harness] の三項組を Read し目的関数とする。加えて [#maintainability-harness] の境界条件のみ Read し侵食禁止制約とする。分析・指摘は三項組への因果で記述する。 | `scripts/scan-claude-md.py` | `references/claude-md.md` | → 昇華チェック（下記） |
| 3 | `docs/harness-engineering-domain-model.md` の [#maintainability-harness] の三項組を Read し目的関数とする。加えて [#architecture-fitness-harness] の境界条件のみ Read し侵食禁止制約とする。分析・指摘は三項組への因果で記述する。 | `scripts/scan-skills.py` | `references/skills.md`, `references/skill-design-patterns.md` | - |

Agent 1 は scan-metrics.py の tool 別 PermissionRequest カウントを permissions.allow / guard との相関分析に使用する。Agent 3 の scan-skills.py は skill_usage を自身で集計するため、投資対効果評価は Agent 3 内で完結する。

**Agent 2 追加指示**: memory 昇華チェック。scan-config.py 出力の `encoded_cwd` から `~/.claude/projects/{encoded_cwd}/memory/` を探索し、MEMORY.md と各トピックファイルを Read。下記テーブルに従い昇華・削除・維持の候補リストを出力する

| メモリタイプ | 昇華先 | 条件 | ドメイン目的関数判定基準 |
|------------|-------|------|----------------------|
| feedback（全体共通ルール） | CLAUDE.md | 全プロジェクトに適用、CLAUDE.mdに未記載 | `#behaviour-harness` の達成シグナルに寄与するか |
| feedback（パス固有ルール） | rules/ | 特定ファイル種別・ディレクトリに限定 | 該当ドメイン（例: coding-principles rule なら `#maintainability-harness`）の達成シグナルに寄与するか |
| feedback（CLAUDE.mdと重複） | 削除 | すでにCLAUDE.mdに同内容がある | 昇格対象外 |
| project（永続的な方針・制約） | CLAUDE.md or プロジェクトCLAUDE.md | 時限性がなく全般適用 | 昇格対象外 |
| reference（外部リソース情報） | skills/references/ | 特定スキルに紐づく | 昇格対象外 |
| 繰り返し参照されるワークフロー | skills/ | 手順として定型化できる | 昇格対象外 |
| user型・一時的projectメモ | 維持 | 個人情報や一時的な作業状態 | 昇格対象外 |

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
独立レビュー → issue判定 → 修正 → 次サイクル or 完了
```

**独立レビュー（サブエージェント委譲・opus）**:

Task tool (opus) で委譲。プロンプト構造:
- **目的関数**: `~/.claude/skills/_shared/independent-review-prompt.md` を Read し適用する（harness 本体レビューなので #architecture-fitness-harness 主分岐が適用される）。
- **コンテキスト**: モード（`${診断モード | 自己改善モード | 能動的探索モード}`）、changed-files.sh の出力パスリスト
- **指示**: `references/review-criteria.md` を Read し4観点の判定基準を把握。各ファイルの変更箇所を基準に照らして評価。高信号フィルタに該当しない指摘は除外
- **成功基準**: 各指摘が review-criteria.md の4観点いずれかに紐づいている。出力: `issue_id: REVIEW-${cycle}-{sequential}` / `file` / `line` / `dimension` / `problem` / `suggestion`。issueなし→「指摘なし」

**issue判定（自身で実行）**:
- issueなし → 完了サマリーへ
- 前サイクルと同一issue（file+line一致） → `[RECURRING]` マーク、修正対象外
- issueあり → 修正へ

**修正（自身で実行）**: issueごとに Edit tool で適用する（設定ドキュメント修正は軽量なため自身で実行）。最大2サイクル到達時は残存issueを報告し完了サマリーへ。

### R3: レビュー結果ファイル書き出し

レビューを実行した場合、結果を `.claude/tmp/review/YYYY-MM-DD-HHMMSS-{topic}.md` に書き出す。

## セッション終了フェーズ

### L6-c: 診断ログ出力

各モードの完了サマリー出力後、判断ポイントごとに1 JSONLレコードを `.claude/tmp/diagnostic-trace/{YYYY-MM-DD-HHMMSS}-{session-id}.jsonl` へ追記する。

スキーマ定義: `.claude-global/skills/retrospective/output-schema.md`

**出力タイミング**: 判断が発生したステップ（scan結果の解釈、agent出力の統合、issue判定、適用確認等）ごとに即時追記する。セッション終了時にまとめて書き出す方式は禁止（判断脱落防止のため）。

**最低記録対象**:

| スキルフェーズ | 記録する判断の例 |
|-------------|---------------|
| Step 3（統合・優先度付け） | Critical/High 判定根拠 |
| Step 4（適用確認） | ユーザー選択への応答解釈 |
| R2（独立レビュー・issue判定） | issue 採否の判定根拠 |

**出力手順**:

1. セッション開始時に `session_id` を `{YYYY-MM-DD-HHMMSS}-${CLAUDE_SESSION_ID}` 形式で確定する
2. 出力先ディレクトリ `.claude/tmp/diagnostic-trace/` が存在しない場合は Bash tool で作成する
3. 各判断ポイントで output-schema.md のフィールド定義に従い JSON オブジェクトを構築し、Bash tool で追記する:
   ```bash
   echo '{"timestamp":"...","session_id":"...","skill":"claude-config","phase":"Step 3","judgment_type":"reference","reference_cited":"...","value_stated":null,"rationale":"...","tool_used":"...","context_size":null}' \
     >> .claude/tmp/diagnostic-trace/{YYYY-MM-DD-HHMMSS}-{session-id}.jsonl
   ```
4. 任意フィールドも可能な限り記録する（output-schema.md の採用原則に従う）

---

## 注意事項

- スクリプト実行: `~/.dotfiles/.claude-global/skills/claude-config/scripts/`（実ファイルパス。.dotfiles 配置前提）
- ドキュメント参照: `~/.claude/skills/claude-config/references/`（シンボリックリンク経由）
