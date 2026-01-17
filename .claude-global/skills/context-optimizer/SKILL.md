---
name: context-optimizer
description: |
  CLAUDE.mdを最小化し、コンテキスト消費を最適化。
  「CLAUDE.md圧縮」「コンテキスト最適化」「context-optimizer」と依頼された時に使用。
user-invocable: true
---

# Context Optimizer

CLAUDE.mdを50行未満に削減し、コンテキスト消費を最適化するスキル。

## 目標

| 段階 | 行数 | 説明 |
|-----|-----|------|
| 理想 | 20-30行 | 役割・経験・核心的指針のみ |
| **目標** | **50行未満** | 推奨 |
| 許容 | 150行以下 | 公式上限 |

## ワークフロー（6段階）

### Phase 1: 分析

1. 対象ファイルを特定
   - CLAUDE.md（プロジェクト直下）
   - 既存の `.claude/` 配下（rules/skills/commands/agents）
2. 行数カウント（`scripts/analyze-context.sh` を使用）
3. 各セクションの内容を把握

```bash
# 分析スクリプト実行
~/.dotfiles/.claude-global/skills/context-optimizer/scripts/analyze-context.sh [target_dir]
```

### Phase 2: 分類

各セクションに判断基準を適用:

| コンテンツ種別 | 判断基準 | 抽出先 |
|--------------|---------|--------|
| パス固有ルール | 特定の拡張子・ディレクトリにのみ適用 | `rules/{topic}.md` |
| ワークフロー | 3ステップ以上の手順 | `skills/{name}/SKILL.md` |
| 定型プロンプト | ユーザー明示実行（`/command`） | `commands/{name}.md` |
| 専門タスク | 独立コンテキスト必要 | `agents/{name}.md` |
| 核心的指針 | 全操作に必須 | CLAUDE.mdに残す |

詳細: `~/.claude/skills/context-optimizer/references/extraction-criteria.md`

### Phase 3: 計画

抽出計画表を作成し、ユーザーに提示:

```markdown
| Content | Extract To | Type | Trigger/Path |
|---------|------------|------|--------------|
| TypeScript規約 | rules/typescript.md | Rule | `**/*.ts` |
| コードレビュー手順 | skills/code-review/SKILL.md | Skill | 「レビューして」 |
| コミット作成 | commands/commit.md | Command | `/commit` |
```

**必ず `AskUserQuestion` で承認を取得してから次へ進む。**

### Phase 4: 抽出

承認後、抽出ファイルを生成:

1. 適切なfrontmatter付きでファイル作成
2. 正しいディレクトリに配置（rules/skills/commands/agents）
3. dotfiles環境の場合はシンボリックリンクを確認

frontmatter例: `~/.claude/skills/context-optimizer/templates/frontmatter-examples.md`

### Phase 5: リファクタ

CLAUDE.mdを圧縮:

1. 抽出した内容を削除
2. 残すべき内容のみに整理
3. 必要に応じて抽出先への参照を追加

**残すべき内容**:
- プロジェクト概要（1-2文）
- 技術スタック（箇条書き）
- 重要コマンド（最重要のみ）
- 核心的コーディング原則（3-5項目）

テンプレート: `~/.claude/skills/context-optimizer/templates/minimal-claude-md.md`

### Phase 6: 報告

最適化結果を報告:

```markdown
## Before/After

| 項目 | Before | After | 削減率 |
|-----|--------|-------|-------|
| CLAUDE.md行数 | {before} | {after} | {rate}% |

## 作成ファイル一覧
- rules/xxx.md
- skills/xxx/SKILL.md
```

## チェックリスト

- [ ] CLAUDE.mdが50行未満になっている
- [ ] 抽出ファイルに適切なfrontmatterがある
- [ ] rulesに`paths:`が記載されている
- [ ] skillsに`name:`と`description:`が記載されている
- [ ] descriptionにトリガー説明（「〜時に使用」）がある

## 検証手順

1. 新しいセッションを開始
2. `claude --print-system-prompt` でコンテキスト確認
3. 各スキルが適切にトリガーされるか確認

## ロールバック

```bash
git checkout HEAD -- CLAUDE.md .claude/
```

## 参照ファイル

| ファイル | 用途 |
|---------|------|
| `references/extraction-criteria.md` | 抽出先判断基準の詳細 |
| `references/anti-patterns.md` | 避けるべきパターン |
| `templates/minimal-claude-md.md` | 最小CLAUDE.mdテンプレート |
| `templates/frontmatter-examples.md` | frontmatterテンプレート |
| `templates/optimization-report.md` | レポートテンプレート |
| `scripts/analyze-context.sh` | 行数分析スクリプト |
