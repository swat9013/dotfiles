---
paths: .claude-global/settings.json, .claude-global/hooks/**, .claude-global/skills/claude-config/**
---

# CONFIG-MANAGEMENT: 設定管理系 コナセンス仕様

各設定要素の実装詳細は各ファイル/SKILL.md を参照。本ドキュメントは単体では読み取れない連動契約（コナセンス）を記述する。

**責務分担ポインタ**: 構造の概要 → CLAUDE.md、配置判断 → context-architecture.md（references/）、診断・自己改善 → `/claude-config`、統治原則 → harness-architecture.md

**更新タイミング**: コナセンスに該当する設定を変更したとき / hooks スクリプトを追加・削除したとき / permissions 体系を変更したとき

## 1. コナセンス・マップ

### 強連動（同時更新しないと壊れる）

| コナセンス | 連動箇所 | 壊れ方 |
|-----------|---------|--------|
| **settings.json command パス** | settings.json の hooks[*].command, fileSuggestion, statusLine ↔ 実スクリプトの存在・実行権限 | スクリプトリネーム/移動→コマンドパス不一致でhook/UI機能が無効化。シンボリックリンク先の実パス（`~/.dotfiles/.claude-global/...`）で記述必須 |
| **permissions.allow スクリプトパス** | settings.json permissions.allow ↔ 実スクリプトの存在 | スクリプト追加時にallow未更新→実行ブロック。allow追加で実体なし→空振り（エラーにならないため気づきにくい） |
| **二層防御の対応** | guard-*.sh（第1層）↔ permissions.deny（第2層） | guard スクリプトにルール追加してもdenyに未反映→hook未起動時（Task toolバイパス等）のフォールバック欠落。逆にdenyのみ追加でguard が未対応→カスタムエラーメッセージなし |

### 中連動（整合性確認が必要）

| コナセンス | 連動箇所 | 影響 |
|-----------|---------|------|
| **スキル設計ルールの重複保有** | claude-config/references/skill-design-patterns.md ↔ claude-config/references/skills.md | 両ファイルが「description 130字以下」「500行制限」等の判定基準を保有。一方だけ更新→診断基準と設計ガイドが乖離。正規ソースは `rules/claude-global-skills.md` |
| **Gotchas 追記ルール** | claude-config/references/update-guide.md（「Gotchas は該当ドメインの rules/ ファイルに追記」と明示） | 新スキル作成時に発見したGotchasの追記先が案内されない |
| **skill-activation.sh の3ステップ構造** | hooks/skill-activation.sh の出力テキスト（EVALUATE/ACTIVATE/IMPLEMENT）↔ 全スキルのSKILL.md description設計 | 出力テキスト変更→スキル自動選択精度が変化。hook無効化→選択率が大幅低下。description の最適化はhook有効を前提としている |
| **todoist-refine のスクリプトパス参照** | todoist-refine/SKILL.md のスクリプトパス ↔ todoist/scripts/todoist.py の実体パス | todoist.py のパス変更時に todoist-refine 側が壊れる（コマンド不在エラー） |
| **researcher 調査原則参照** | claude-config/SKILL.md Step A2（`Read researcher/SKILL.md` で調査原則を展開）↔ researcher/SKILL.md | researcher/SKILL.md の調査原則セクション構造変更時に Step A2 の展開ロジックが壊れる |
| **scan スクリプト分割** | scan-config.py（共通コンテキスト・encoded_cwd）↔ scan-hooks.py + scan-metrics.py（Agent 1、セッション定量含む）↔ scan-claude-md.py（Agent 2）↔ scan-skills.py（Agent 3）↔ memory/（Agent 2 統合） | スクリプト追加/移動時に SKILL.md の Agent テーブルと不整合。scan-config.py の出力は全 Agent に共有される前提 |

## 2. 変更ガイド

### hook スクリプトを追加/移動するとき

1. `settings.json` hooks[*].command を実パスで更新
2. `settings.json` permissions.allow にパスを追加
3. 禁止ルールを含む場合: 該当の `guard-*.sh` にチェック追加（git→guard-git.sh、shell→guard-shell.sh、Write/Edit→pretooluse-guard-write.sh）
4. 禁止ルールを含む場合: `permissions.deny` にも同等ルールを追加（二層防御の完全性）

### scan スクリプトを追加/移動するとき

1. SKILL.md の Agent テーブル（Step 2）を更新
2. config-management.md のコナセンス「scan スクリプト分割」行を更新

### permissions.deny にルールを追加するとき

- 該当の `guard-*.sh` にも同等のチェックを追加する（hook 未起動時のフォールバック保証）

### スキル設計ルールを変更するとき

1. `rules/claude-global-skills.md`（正規ソース）を更新
2. `claude-config/references/skill-design-patterns.md` と `claude-config/references/skills.md` の整合性を確認
3. 判断が難しい場合は `/claude-config` で診断

### 新しい Gotchas を発見したとき

- 該当ドメインの `rules/` ファイルの Gotchas セクションに追記（例: lib関連 → `lib-scripts.md`、スキル関連 → `claude-global-skills.md`）

### パス参照の規則

| 用途 | パス形式 | 理由 |
|------|----------|------|
| ドキュメント参照 | `~/.claude/skills/...` | シンボリックリンク経由で環境非依存 |
| スクリプト実行 | `~/.dotfiles/.claude-global/...` | シェル実行時の確実性 |
| settings.json command | `~/.dotfiles/.claude-global/...` | 実ファイルパス必須 |

## 3. 迷ったときは

設定の配置判断や横断的な矛盾検出が必要な場合は `/claude-config` で診断モードを実行する。自動注入されるコナセンス・マップは「何を一緒に変えるか」の契約。「どこに何を書くべきか」の判断は claude-config の診断モードが担う。
