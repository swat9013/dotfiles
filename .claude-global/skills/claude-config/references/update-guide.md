# 自己改善モード 手順ガイド

## カテゴリ分類と更新先マッピング

| カテゴリ | 更新先 | 対象ファイル例 |
|---------|--------|-------------|
| settings（一般） | `rules/` | `project-settings.md` |
| settings（permissions） | `references/` | `settings.md`（permissions モデル・allow 構文） |
| rules（paths/frontmatter） | `rules/` | `project-rules.md` |
| hooks | `references/` | hooks 系 references |
| subagent / Task tool | `references/` | subagent 系 references |
| agent-teams | `references/` | `agent-teams.md` |
| task-management | `references/` | task 系 references |
| skills（SKILL.md設計） | `rules/` | `project-skills.md` |
| claude-md（CLAUDE.md設計） | `references/` | claude-md 系 references |
| context-architecture | `references/` | overview 系 references |
| Gotchas（環境制約・既知バグ） | `rules/` | 該当ドメインの rules/ ファイルの Gotchas セクション |

複数カテゴリにまたがる場合は両方を更新する。

---

> このガイドは自己改善モード（`update <URL>`）と能動的探索モード（`update auto`）の両方から参照される。

## 手順

### 1. 入力を取得する

- **URL**: WebFetch で取得する
- **テキスト**: ユーザーが貼り付けた内容をそのまま使用する

### 2. 記事内容を分析しカテゴリを特定する

記事から以下を読み取る:

- 主題（settings / hooks / skills / subagent / Gotchas など）
- 新情報か既知情報の補足か
- 上のマッピング表で更新先を特定する

### 3. 更新先ファイルを Read で確認する

- 対象ファイルを Read で読み込む
- 追加・修正・削除すべき差分を特定する
- 重複する記述がすでに存在する場合は更新不要と判断する

### 4. Edit で差分のみ反映する

- 全文書き換えは禁止。Edit で差分のみ適用する
- 既存の構造・フォーマット（表/リスト/見出し階層）に合わせる
- Gotchas の追加は該当ドメインの rules/ ファイルの `## Gotchas` セクション末尾に箇条書きで追記する

### 5. 整合性チェックを実施する

1. `references/` のファイルに `rules/` と重複する情報が含まれていないか確認する
2. `references/` と `rules/` で矛盾する記述がないか確認する
3. 更新したファイルが 200 行を超えていないか確認する（超過時は分割を検討する）

### 6. 変更サマリーを出力する

```
## 自己改善結果

### 更新内容
- [更新先ファイル]: [追加/修正/削除した情報の要約]

### 整合性チェック
- references と rules の重複: なし / [重複箇所]
- 矛盾する記述: なし / [矛盾箇所]
- 行数制限: OK / [超過ファイル]
```

---

## 判断基準

| 状況 | 対応 |
|------|------|
| 既存の記述と完全に同じ内容 | 更新不要。サマリーに「変更なし」と記載 |
| 既存記述を上書きする新情報 | Edit で該当箇所を置き換える |
| 既存にない新情報 | 適切なセクション末尾に追記する |
| Gotchas 系の環境制約・既知バグ | 該当ドメインの rules/ ファイルの `## Gotchas` に追記 |
| rules/ にすでにある情報を references/ に書こうとした場合 | references/ への追記をやめ、rules/ の既存記述で十分か確認する |
