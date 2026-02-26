# 最適化レポートテンプレート

最適化完了後に生成するレポートの形式。

---

## テンプレート

```markdown
# コンテキスト最適化レポート

## 概要

- 実行日時: {YYYY-MM-DD HH:MM}
- 対象: {target_path}

## Before/After

| 項目 | Before | After | 削減率 |
|-----|--------|-------|-------|
| CLAUDE.md行数 | {before_lines} | {after_lines} | {reduction}% |

## 抽出計画

| Content | Extract To | Type | Trigger/Path |
|---------|------------|------|--------------|
| {content_1} | {extract_to_1} | {type_1} | {trigger_1} |
| {content_2} | {extract_to_2} | {type_2} | {trigger_2} |
| ... | ... | ... | ... |

## 作成ファイル一覧

- `{file_1}` - {description_1}
- `{file_2}` - {description_2}
- ...

## 検証チェックリスト

- [ ] CLAUDE.md が50行未満になっている
- [ ] 抽出ファイルに適切なfrontmatterがある
- [ ] rulesに`paths:`が記載されている
- [ ] skillsに`name:`と`description:`が記載されている
- [ ] descriptionに「〜時に使用」相当の説明がある

## 検証手順

1. 新しいセッションを開始
2. `claude --print-system-prompt` でコンテキスト確認
3. 各スキルが適切にトリガーされるか確認

## ロールバック

必要な場合は git で復元:

```bash
git checkout HEAD -- CLAUDE.md .claude/
```
```

---

## 例: 実際のレポート

```markdown
# コンテキスト最適化レポート

## 概要

- 実行日時: 2026-01-17 15:30
- 対象: /Users/user/project

## Before/After

| 項目 | Before | After | 削減率 |
|-----|--------|-------|-------|
| CLAUDE.md行数 | 250 | 35 | 86% |

## 抽出計画

| Content | Extract To | Type | Trigger/Path |
|---------|------------|------|--------------|
| TypeScript規約 | rules/typescript.md | Rule | `**/*.ts` |
| API設計ガイド | rules/api-design.md | Rule | `src/api/**` |
| コードレビュー手順 | skills/review-fix/SKILL.md | Skill | 「レビューして」 |
| デバッグ手順 | skills/debug/SKILL.md | Skill | 「デバッグして」 |
| コミット作成 | commands/commit.md | Command | `/commit` |

## 作成ファイル一覧

- `rules/typescript.md` - TypeScript型付け・命名規約
- `rules/api-design.md` - RESTful API設計ガイドライン
- `skills/review-fix/SKILL.md` - 5観点コードレビュー手順
- `skills/debug/SKILL.md` - 体系的デバッグ手順
- `commands/commit.md` - Conventional Commits形式でコミット

## 検証チェックリスト

- [x] CLAUDE.md が50行未満になっている（35行）
- [x] 抽出ファイルに適切なfrontmatterがある
- [x] rulesに`paths:`が記載されている
- [x] skillsに`name:`と`description:`が記載されている
- [x] descriptionに「〜時に使用」相当の説明がある

## 検証手順

1. 新しいセッションを開始
2. `claude --print-system-prompt` でコンテキスト確認
3. 各スキルが適切にトリガーされるか確認

## ロールバック

必要な場合は git で復元:

```bash
git checkout HEAD -- CLAUDE.md .claude/
```
```

---

## 出力時の注意

1. **削減率の計算**
   ```
   削減率 = (before - after) / before * 100
   ```

2. **抽出計画表の Type**
   - Rule: パス固有ルール
   - Skill: ワークフロー
   - Command: 定型プロンプト
   - Agent: 専門タスク

3. **Trigger/Path の形式**
   - Rule: glob パターン（`**/*.ts`）
   - Skill: トリガーキーワード（「〜して」）
   - Command: スラッシュコマンド（`/commit`）
   - Agent: トリガーキーワード
