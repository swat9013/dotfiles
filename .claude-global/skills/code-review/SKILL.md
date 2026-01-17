---
name: code-review
description: コードレビュー実行と学習反映。「コードレビュー」「レビューして」「review-update」と依頼された時に使用。5観点（セキュリティ、信頼性、パフォーマンス、アーキテクチャ、品質）で評価。
---

# Code Review

コードレビューを実行し、優先度別フィードバックを提供するスキル。

## モード選択

| トリガー | モード |
|---------|--------|
| /code-review、コードレビュー、レビューして | Review |
| /review-update、学びを反映 | Update |

---

## Review モード

### 前提条件

1. レビュー対象ファイルが存在
2. CLAUDE.md が読み込み可能

### 実行手順

#### 1. 対象特定
- 大規模変更（50ファイル超 or 1000行超）→ Critical/High先行レビューを提案

#### 2. ドメイン知識収集
- CLAUDE.md、docs/ から制約・ドメイン知識を確認

#### 3. 自動チェック
- テスト、Lint、ビルド実行。**失敗時は中止**

#### 4. レビュー実行

5観点のチェックリストでレビュー:

| 観点 | チェックリスト |
|------|--------------|
| security | `~/.claude/skills/code-review/checklists/security.md` |
| reliability | `~/.claude/skills/code-review/checklists/reliability.md` |
| performance | `~/.claude/skills/code-review/checklists/performance.md` |
| architecture | `~/.claude/skills/code-review/checklists/architecture.md` |
| quality | `~/.claude/skills/code-review/checklists/quality.md` |

**Task tool対応の場合（Claude Code）**:

**必須**: 5つのTask toolを**単一メッセージで並列実行**

自分で直接レビューしない。必ずsub-agentに委譲する。

各promptの構造:
```
あなたは${観点}専門のコードレビュアーです。

原則:
${guides/code-review-agents.mdから該当ロールをコピー}

## チェックリスト
${checklists/code-review/${観点}.mdの内容}

## レビュー対象
${対象コード}

## 出力
templates/code-review-output.md に従い、担当観点のみ報告
```

→ ロール定義: `~/.claude/skills/code-review/guides/agents.md`

**Task tool非対応の場合**:

5観点を**順番に**レビュー（観点ごとにロール切替）

#### 5. 結果統合
- 優先度順（Critical→High→Medium→Low）に再編成
- 同一箇所への指摘は統合、重複排除

#### 6. 自己検証

→ `~/.claude/skills/code-review/guides/self-reflection.md`

指摘を以下で再検証:
- 位置検証: 行番号に該当コードが存在するか
- 構文検証: 修正案が構文エラーにならないか
- 重複排除: 同一問題への複数指摘を統合
- 過剰指摘フィルタ: Lint自動修正可能、スタイル好みは除外

#### 7. 結果出力

→ `~/.claude/skills/code-review/templates/output.md`

### 出力形式

```markdown
# コードレビュー結果: [対象]

## Critical
### 1. `file.ts:42` - [問題] (確信度: 9/10)
**観点**: security
**問題**: [具体的な問題点]
**根拠**: [コード引用]
**修正案**:
| 選択肢 | 概要 | 理由 |
|--------|------|------|
| A (推奨) | [修正案] | [理由] |

## High / Medium / Low
（同様の形式）

## 総評
指摘: Critical X, High X, Medium X, Low X
次のアクション: [推奨事項]
```

### 確信度の基準

| スコア | 意味 | 例 |
|--------|------|-----|
| 9-10 | 明確な問題 | バグ、セキュリティ脆弱性 |
| 7-8 | 高確度で問題 | N+1クエリ、未処理例外 |
| 5-6 | 改善推奨 | 可読性低下、責務混在 |
| 3-4 | 提案レベル | 命名改善、リファクタリング候補 |

### 成功基準

1. 優先度別に整理
2. 確信度・根拠・修正案を含む
3. 自動チェック通過
4. 自己検証実施済み

---

## Update モード

コードレビューセッションの学びをチェックリスト・コマンドに反映。

### 前提条件

1. コードレビューセッションが直前に実施された
2. 保存すべき学びがセッション内に存在する

### 実行手順

#### 1. 抽出
セッションから保存すべき内容を特定:
- 新しいチェック観点
- 有効だったパターン・アンチパターンの例
- 繰り返し指摘した問題
- レビューフローの改善点

#### 2. 整合性チェック
- 既存項目との矛盾・重複がないか確認
- 優先度（Critical/High/Medium/Low）の妥当性確認

#### 3. 保存先判断

| 種類 | 保存先 |
|-----|-------|
| チェック項目・パターン例 | `_shared/checklists/code-review/` |
| レビューフロー | このSKILL.md |

#### 4. 出力

```
### 変更サマリー
- チェックリスト: 追加X件、修正X件
- 圧縮: 実施/不要

### 更新内容
[差分または追加項目]
```

### 原則

- 具体的な修正例（Good/Bad）を重視
- 実践に必要な知識のみ記載
- 汎用的なベストプラクティスをチェックリストへ

---

## よくあるパターン

→ `~/.claude/skills/code-review/templates/patterns.md`

| パターン | 問題 | 修正 |
|---------|------|------|
| N+1クエリ | ループ内でクエリ | Eager Loading |
| エラーハンドリング | レスポンスチェックなし | ステータス確認 |
| 依存注入 | 依存を直接生成 | コンストラクタ注入 |
| テスト | Stubの呼び出し検証 | 最終結果のみ検証 |
