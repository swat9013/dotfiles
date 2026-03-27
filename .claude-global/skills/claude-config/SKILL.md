---
name: claude-config
description: Claude Code設定の診断・最適化と、ベストプラクティス記事からの自己改善。Use when「設定を最適化」「config」「Claude設定チェック」「ベストプラクティス反映」。
disable-model-invocation: true
allowed-tools: Read, Glob, Grep, Bash, Edit, Write, AskUserQuestion, WebFetch, Task
argument-hint: "[optimize | update <URL or text>]"
---

# claude-config

## 概要

Claude Code 設定を診断・最適化するスキル。2つのモードを持つ。

- **診断モード**: プロジェクトの `.claude/` 設定をスキャンし、ベストプラクティスに照らして改善提案
- **自己改善モード**: 新しいベストプラクティス記事を渡すと、スキルのナレッジ（references/ と rules/）を更新

## 知識の分担

| 格納先 | 担当カテゴリ | 読み込みタイミング |
|--------|------------|-----------------|
| `.claude-global/rules/` | settings, rules のベストプラクティス | パスマッチ時に自動読込 |
| `references/` | hooks, subagent, task-management, skills, claude-md, context-architecture | スキル実行時に参照 |

重複ゼロ原則: 両者に同じ情報を書かない。

## Step 0: モード判定

引数 `$ARGUMENTS` を確認:

- `update` で始まる → **自己改善モード**（Step U へ）
- それ以外（空含む）→ **診断モード**（Step 1 へ）

---

## 診断モード

### Step 1: スキャンスクリプト実行

Bash tool でスクリプトを実行し、構造化テキスト出力を取得する。

```
~/.dotfiles/.claude-global/skills/claude-config/scripts/scan-config.sh
```

出力が空や異常な場合は、スクリプトの存在・実行権限を確認する。

### Step 2: AI分析

スクリプト出力のカテゴリに応じて参照先を使い分ける:

| カテゴリ | 参照先 |
|---------|--------|
| `settings`, `rules` | rules/ 自動読込済み。追加 Read 不要 |
| `hooks` | Read `references/hooks.md` |
| `subagent` | Read `references/subagent-orchestration.md` |
| `task-management` | Read `references/task-management.md` |
| `skills` | Read `references/skills.md` |
| `claude-md` | Read `references/claude-md.md` |
| `context-architecture` | Read `references/context-architecture.md` |

スキャン出力に出現したカテゴリの references のみ読む（不要な Read を省く）。

### Step 3: 改善提案の出力

優先度付きテーブル形式で出力:

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

- 適用後に変更サマリーを出力
- 適用できない項目（手動確認が必要な場合）はその旨を明記

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

## 注意事項

- スクリプト実行: `~/.dotfiles/.claude-global/skills/claude-config/scripts/`（実ファイルパス。.dotfiles 配置前提）
- ドキュメント参照: `~/.claude/skills/claude-config/references/`（シンボリックリンク経由）
