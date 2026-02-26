---
name: code-review
description: コードレビュー実行。4観点（Security、Correctness、Design、Testing）で評価。「コードレビュー」「レビュー」「review」「コード確認」「コードチェック」「PRレビュー」「プルリクレビュー」「差分レビュー」「品質チェック」と依頼された時に使用。
disable-model-invocation: true
---

# Code Review

コードレビューを実行し、高信号フィードバックを提供するスキル。

## 前提条件

1. レビュー対象ファイルが存在
2. CLAUDE.md および関連rules/が読み込み可能

## 実行手順

### Step 1: 前処理チェック（スクリプト実行）

Bash toolでスクリプトを直接実行し、適格性をチェック:

```bash
~/.dotfiles/.claude-global/skills/scripts/changed-files.sh
```

出力の `RESULT` で判定:
- `NO_CHANGES` → 「レビュー対象の変更がありません」と伝えて終了
- `TOO_LARGE` → 分割レビューを提案
- `CONFIG_ONLY` → 「設定ファイルのみの変更のためレビュー不要」と伝えて終了
- `PROCEED` → `--- File List ---` 以降のファイル一覧をStep 2のレビュー対象として使用

**中止条件に該当した場合、理由を説明して終了**

### Step 2: 並列レビュー（4観点）

**必須**: 4つのTask toolを**単一メッセージで並列実行**

自分で直接レビューしない。必ずサブエージェントに委譲する。

| 観点 | モデル | チェックリスト |
|------|--------|----------------|
| security | Opus | `checklists/security.md` |
| correctness | Opus | `checklists/correctness.md` |
| design | Sonnet | `checklists/design.md` |
| testing | Sonnet | `checklists/testing.md` |

各promptの構造:
```
あなたは${観点}専門のコードレビュアーです。

## ロール
${guides/agents.mdから該当ロールをコピー}

## チェックリスト
${checklists/${観点}.mdの内容}

## 高信号フィルタ
フラグすべき:
- コンパイル/パースエラー、型エラー
- 明確なロジックエラー
- セキュリティ脆弱性（SQLインジェクション、XSS等）
- CLAUDE.md/rules違反（引用可能なもの）

フラグしない:
- コードスタイル
- 潜在的問題（入力依存）
- Linterがcatchする問題
- 既存コードの問題（新規導入ではない）

## レビュー対象
${対象コード}

## 出力形式
| ファイル:行 | 問題 | 修正案 |
```

### Step 3: 検証・フィルタリング

Step 2で検出されたissueを別サブエージェントで再検証:

- security/correctness の issue → Opus
- design/testing の issue → Sonnet

検証手順は `guides/self-reflection.md` に従う。

**検証されないissueは破棄。同一箇所への複数指摘は統合。**

### Step 4: 出力

`templates/output.md` の形式に従い、**カレントディレクトリに `review.md` として書き出す**。

既存の `review.md` がある場合は上書きする。

## 成功基準

1. 高信号issueのみを報告
2. 根拠・修正案を含む
3. 検証済み（偽陽性を排除）

## よくあるパターン

→ `templates/patterns.md`
