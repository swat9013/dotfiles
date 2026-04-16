---
name: harness-tuning
description: claude-config の scripts/Agent指示/references を診断し修正を提案する。Use when「Steering Loop が浅い」「診断が機能しない」「Agent出力が不十分」「harness-tuning」。
disable-model-invocation: true
allowed-tools: Read, Glob, Grep, Bash, Edit, Write, AskUserQuestion
model: opus
effort: high
argument-hint: "[問題の概要]"
---

# harness-tuning

claude-config コンポーネント（スクリプト/Agentプロンプト/references）を診断し、修正案を提示・適用する。

**目的関数**: `docs/harness-engineering-domain-model.md §2.2`（Architecture Fitness Harness）の三項組を Read し目的関数とする。加えて `§2.3` の境界条件のみ Read し侵食禁止制約とする。すべての仮説・修正提案は三項組への因果（harness 自身の劣化を検出・修正し続ける最大化価値を損なう / 達成シグナルを変化させる / Behaviour境界条件を侵害する）で記述する。

**判断基準 reference**:
- `references/tuning-guide.md` — 10分類の修正パターンテーブル（Cat-5以降は `tuning-guide-extended.md`）
- `~/.dotfiles/docs/harness-engineering-domain-model.md` — §4.2 Steering Loop / §5 計測戦略
- `~/.dotfiles/CLAUDE.md` — Harness Architecture 原則（Computational First / Guide First, Sense Second）

---

## Step 1: 問題把握

`$ARGUMENTS` から「対象・症状・期待」の3要素を抽出する。

- **対象**: どのコンポーネントか（スクリプト / Agentプロンプト / references）
- **症状**: 何が起きているか（出力が浅い / 診断が機能しない / 指摘がない 等）
- **期待**: どうなれば解決か

3要素のいずれかが不足している場合は AskUserQuestion で確認する:

```
どのコンポーネント（スクリプト/Agentプロンプト/references）で何が起きていますか？
期待した出力は何でしたか？実際の出力と比べて何が足りませんでしたか？
```

---

## Step 2: ギャップ特定

`~/.dotfiles/docs/harness-engineering-domain-model.md` を Read し、「あるべき姿」と現実のギャップを特定する。

参照箇所:
- **§4.2 Steering Loop** — 劣化検出→診断→修正→検証のサイクル。3つの知見源（retrospective/逸脱FB/独立レビュー）の責務分離
- **§5 計測（Observability）戦略** — 計測対象①②③と2段構えの実装（Tier1/Tier2）

ギャップの記録フォーマット:
```
- あるべき姿: [domain-model の記述]
- 現実: [実際の動作]
- ギャップ: [差分の要約]
- Computational First 違反: [有/無 + 該当箇所。skill内LLM処理/rules内決定論的規則の script・lint 化余地]
```

Computational First 違反チェックは Cat-9/Cat-10 の検出トリガー。対象コンポーネント（skill SKILL.md / rules/*.md）を読み、決定論的処理が LLM に残っている箇所を抽出する。判定基準は `references/tuning-guide-extended.md` の Cat-9 表・Cat-10 表を参照。

domain-model.md に記述がない事項は「追記候補」として分離して記録する。

---

## Step 3: 仮説生成

`references/tuning-guide.md` の10分類テーブル（Cat-5以降は `tuning-guide-extended.md`）を Read し、Step 1 で特定した対象ファイルを Read して仮説を生成する。

Step 2 で「Computational First 違反: 有」と記録した場合は、該当箇所に対し Cat-9（skill 内 LLM 処理の script 化）/ Cat-10（rules の linter/formatter 化）を**優先的に**仮説化する。これらは構造的な自動化機会であり、他のカテゴリ（出力精度・Agent プロンプト調整等）より目的関数への寄与が大きいことが多い。

各仮説には以下を付記する:
```
- 分類番号: [1-10]
- 対象ファイル: [パス]
- 問題: [具体的に何が問題か]
- 根拠: [domain-model の記述 or tuning-guide の基準]
```

仮説が複数ある場合は、目的関数（ユーザーの説明コスト削減）への影響が大きい順に並べる。

---

## Step 4: 修正案提示 + ユーザー承認

仮説を以下の表形式で提示する:

```
| # | 分類 | 対象ファイル | 問題 | 修正案 | コナセンス影響 |
|---|------|------------|------|--------|--------------|
| 1 | ... | ... | ... | ... | ... |
```

- **コナセンス影響**: `~/.dotfiles/.claude/rules/config-management.md` を参照し、連動変更が必要な箇所を記載。影響なしの場合は「なし」
- **分類**: `references/tuning-guide.md` の8分類番号と名称

表を提示した後、AskUserQuestion で承認を確認する:

```
上記の修正案を適用しますか？
- all: 全項目を適用
- 番号列挙（例: 1,3）: 該当項目のみ適用
- none: 中止

推奨: [最も効果が高い項目番号と理由]
```

---

## Step 5: 適用

Step 4 でユーザーが承認した項目のみ Edit/Write で適用する。

### 適用フロー

1. 承認された番号を確認（`all` の場合は全項目）
2. 対象ファイルを Edit または Write で修正
3. スクリプト変更がある場合は Bash で動作確認:
   - 構文エラーがないか確認（`python -c "import ast; ast.parse(open('file').read())"` 等）
   - 実行権限が必要な場合は `chmod +x` を確認
4. `~/.dotfiles/.claude/rules/config-management.md` のコナセンス連動箇所を確認し、連動変更が必要な箇所を報告
5. **domain-model 整合性の確認**: Step 2 で記録した「追記候補」を `~/.dotfiles/docs/harness-engineering-domain-model.md` に反映する。references/SKILL.md の変更で導入した新概念・新カテゴリが domain-model に欠落している場合、この Step で追記まで完遂させる。追記候補を未解消のまま完了報告しない（Steering Loop の自己メンテナンス責務）。

### 完了報告フォーマット

```
## 適用完了

| # | 対象ファイル | 変更内容 |
|---|------------|---------|
| 1 | ... | ... |

## コナセンス連動（要確認）
[連動変更が必要な箇所、またはなし]

## domain-model.md 整合性
[Step 5-5 で反映した追記内容、または「追記不要」。「追記候補あり・未反映」の状態では完了としない]
```
