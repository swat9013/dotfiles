---
name: claude-code-release
description: Fetches and summarizes high-impact Claude Code release changes. Use when「リリースノート」「最新バージョン」「アップデート内容」。
user-invocable: true
disable-model-invocation: true
argument-hint: "[バージョン（例: v2.1.90、省略時は最新）]"
allowed-tools: Bash
model: sonnet
effort: medium
---

# Claude Code Release Notes

anthropics/claude-code のリリースノートから高影響変更を日本語で要約する。

## ワークフロー

### Step 1: リリースノート取得

バージョン指定あり（`$ARGUMENTS` が空でない場合）:

```bash
gh release view $ARGUMENTS --repo anthropics/claude-code
```

バージョン指定なし:

```bash
gh release view --repo anthropics/claude-code
```

### Step 2: 影響度分析

取得した全項目を読み、[impact-criteria.md](references/impact-criteria.md) の基準で高影響変更を選別する。

### Step 3: 要約出力

以下の形式で出力する:

```
## Claude Code {バージョン} — 注目の変更点

リリース日: {YYYY-MM-DD}

### 新機能
- **{機能名}**: {1-2文の説明}

### 挙動変更（要注意）
- **{変更内容}**: {既存ユーザーへの影響}

### 重要なバグ修正
- **{概要}**: {影響と修正内容}

### パフォーマンス改善
- **{改善内容}**: {具体的な改善}

---
全変更リスト: {GitHubリリースURL}
```

- 項目が1つもないカテゴリは省略
- 高影響変更がない場合（小規模パッチ）は「軽微な修正のみ」と明記
