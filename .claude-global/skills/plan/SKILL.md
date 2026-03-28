---
name: plan
description: コードベース探索から実装計画を立案する軽量設計エージェント。Use when「/plan」「実装計画」「どう実装する？」「アプローチを考えて」。
argument-hint: "[実装したい機能や変更の概要]"
disable-model-invocation: true
denied-tools:
  - Edit
  - NotebookEdit
---

# Plan Agent

コードベースを探索して実装計画を立案する軽量設計エージェント。

**設計対象**: $ARGUMENTS

`$ARGUMENTS` が空の場合、ユーザーに設計対象を確認してから開始する。

## プロジェクトスナップショット

Phase 1 エージェントへの探索ヒントとして活用:

- **ディレクトリ構造**: !`find . -maxdepth 2 -type d -not -path '*/node_modules/*' -not -path '*/.git/*' 2>/dev/null | head -30`
- **最近の変更**: !`git log --oneline -10 2>/dev/null`
- **主要設定**: !`ls package.json pyproject.toml Cargo.toml go.mod Gemfile pom.xml build.gradle Makefile docker-compose.yml 2>/dev/null || echo "(なし)"`

## ワークフロー

### Phase 0: 前提と目的の整理

**設計判断時の問い**: `_shared/design-questions.md` の該当フェーズを参照。

$ARGUMENTS を分析し、以下を明確にする:

1. **なぜこの変更が必要か**（動機・背景）
2. **何を達成すれば完了か**（完了の定義）
3. **スコープ外は何か**（やらないこと）

$ARGUMENTS から上記が読み取れない場合、AskUserQuestion で確認する。
読み取れる場合はそのまま Phase 1 に進む（確認で止めない）。

Phase 0 の結果は Phase 1・2 のサブエージェントプロンプトに含める。

### Phase 1: コードベース理解

$ARGUMENTS とコードベースの規模に応じて、Explore エージェント（sonnet）を**最大3つ、単一メッセージで並列起動**。

- 1エージェント: タスクが限定的、対象ファイルが明確、小規模な変更
- 複数エージェント: スコープが不明確、複数領域にまたがる、既存パターンの理解が必要

各エージェントには**具体的な探索フォーカス**を指定する。再利用可能な既存の関数・ユーティリティ・パターンを積極的に探す。

```
Task tool:
- subagent_type: Explore
- model: sonnet
- prompt: |
    「$ARGUMENTS」について、[探索フォーカスを具体的に指定]を調査する。

    ## 前提と目的（Phase 0）
    [Phase 0 の結果を全文転記]

    ## プロジェクトスナップショット
    [プロジェクトスナップショットの内容を転記]

    再利用可能な既存実装があれば特に注目して報告する。
```

### Phase 2: 設計検討

Phase 1 の結果を踏まえ、Plan エージェント（sonnet）を起動。

- **デフォルト**: 最低1つの Plan エージェントを起動
- **スキップ**: typo修正、1行変更、単純なリネーム等の trivial なタスクのみ
- **複数（最大3）**: 複数領域にまたがる、大規模リファクタ、エッジケースが多い、異なるアプローチの比較が有益な場合

Phase 1 の調査結果は、各エージェントのプロンプトに**全文転記**する（プレースホルダー禁止）。

```
Task tool:
- subagent_type: Plan
- model: sonnet
- prompt: |
    ## コンテキスト
    設計対象: $ARGUMENTS

    ### 前提と目的（Phase 0）
    [Phase 0 の結果を全文転記]

    ### コードベース調査結果
    [Phase 1 の全エージェント結果を全文転記]

    ## 要件と制約
    [Phase 1 で判明した要件・制約を記述]

    ## タスク
    上記コンテキストに基づき、詳細な実装計画を設計する。
    - 設計概要
    - 主要ファイルと変更箇所（パス付き）
    - 実装ステップ（順序付き）
    - 再利用すべき既存実装（パス付き）
    - トレードオフ
```

複数エージェントを起動する場合、各エージェントに異なる視点を指定する。

### Phase 3: plan file 書き出し

#### 準備

`~/.dotfiles/.claude-global/skills/scripts/claude-output-init.sh plans` を実行。

#### 保存先

`.claude/plans/YYYY-MM-DD-HHMMSS-{topic}.md`

- `{topic}`: $ARGUMENTS から kebab-case で生成（例: `auth-redesign`）
- 日時は `date '+%Y-%m-%d-%H%M%S'` で取得
- ファイル書き出し後に: `~/.dotfiles/.claude-global/skills/scripts/open-in-zed.sh <filepath>` を実行

#### plan file の内容

推奨アプローチのみ記載する（検討した全代替案は不要）。
素早くスキャンできる簡潔さと、実行に十分な詳細さのバランスを取る。

最低限含める内容:

- **Context**: なぜこの変更が必要か（Phase 0 の結果）
- **完了の定義**: 何を達成すれば完了か（スコープ外も明記）
- **推奨アプローチ**: 選定理由を含む
- **主要ファイルと変更箇所**: パス付きリスト
- **再利用する既存実装**: ファイルパス付きで参照
- **実装ステップ**: 順序付き
- **検証方法**: どうテスト・確認するか

**Write tool は plan file（`.claude/plans/` 配下）以外では使用禁止。**

### Phase 4: 品質検証

plan.md 作成後、opus サブエージェント（Task tool, `model: opus`）に品質検証を委譲。生成バイアスを排除するためコンテキスト分離で実行。

**サブエージェントへの入力**:
- 生成した plan.md の全文（Read tool でファイルから読み込み）
- 元の依頼（$ARGUMENTS）と Phase 0 の結果
- レビュー観点の指示（下記5カテゴリ）

```
Task tool:
- model: opus
- prompt: |
    以下の plan file をレビューする。

    ## 元の依頼
    $ARGUMENTS

    ## 前提と目的（Phase 0）
    [Phase 0 の結果を全文転記]

    ## plan file
    [Phase 3 で書き出した plan file を全文転記]

    ## レビュー観点
    1. 設計原則の遵守 — 解決策が複雑なら問いを再定義すべきか、将来のための拡張性が混入していないか、責務が分離されているか
    2. Phase 0 との整合性 — 完了の定義と設計内容が対応しているか、スコープ外に踏み込んでいないか
    3. 実装ステップの具体性 — ファイルパス・変更箇所が特定されているか、順序に論理的根拠があるか
    4. 参照の正確性 — ファイルパスが実在するか、既存実装の参照が正しいか
    5. 検証方法の妥当性 — テスト・確認方法が完了の定義をカバーしているか

    **出力形式**: 問題点（番号付き、重要度 高/中/低 + カテゴリ）→ 良い点 → 修正推奨テーブル（#/重要度/対応）
```

**レビュー後のフロー**:
1. レビュー結果をユーザーに表示
2. 全修正を Edit で plan.md に適用

### フィードバックループ

1. Phase 4 のレビュー結果に基づき修正済みの plan file のパスをユーザーに提示
2. レビュー・フィードバックを依頼
3. フィードバックがあれば修正 → 承認まで繰り返し
4. 承認後、「次のステップ: `/breakdown` で実装タスクに分解できます」と案内

## 注意事項

- 不明点は Phase 0 で AskUserQuestion により確認する（ファイル作成前に解決する）
- Phase 1・2 のサブエージェントに Phase 0 結果を渡す際、要約ではなく**全文転記**する
- Phase 2 の Plan エージェントに Phase 1 結果を渡す際も**全文転記**する
- フォーマルな設計書が必要な場合は `/architect` を案内する
