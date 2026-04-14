# Tuning Guide Extended: Cat-5〜Cat-8

基礎パターン（Cat-1〜Cat-4）と修正チェックリストは [tuning-guide.md](tuning-guide.md) を参照。

## TOC

- [Cat-5: Agent 間の責務分割の問題](#cat-5-agent-間の責務分割の問題)
- [Cat-6: 統合ステップの情報損失](#cat-6-統合ステップの情報損失)
- [Cat-7: 入力データの不足（hook レベル）](#cat-7-入力データの不足hook-レベル)
- [Cat-8: モード設計の問題](#cat-8-モード設計の問題)

---

## カテゴリ別診断パターン（続）

### Cat-5: Agent 間の責務分割の問題

**定義**: 3 Agent（hooks/permissions 分析・CLAUDE.md 分析・スキル分析）の守備範囲境界が不適切で、重複または抜け漏れが生じる。

#### 読むファイル

- `~/.claude/skills/claude-config/SKILL.md`（Step 2 の Agent テーブル全体）

#### 比較基準

- `~/.dotfiles/.claude/rules/config-management.md`（scan スクリプト分割コナセンス）

  > scan-config.py（共通コンテキスト）↔ scan-hooks.py + scan-metrics.py（Agent 1）↔ scan-claude-md.py（Agent 2）↔ scan-skills.py（Agent 3）

- 各 Agent の「目的関数」が守備範囲を一意に定義していること

#### 典型症状

- Agent 1 の出力（permissions 分析）が Agent 3 の出力（スキル使用頻度）と重複している
- Agent 2 が scan-metrics.py の出力（metrics データ）を参照しようとして失敗する
- Agent 3 が permissions の評価を行っている（Agent 1 の責務）

#### 診断手順

1. SKILL.md の各 Agent の「実行スクリプト」列と config-management.md の scan スクリプト分割コナセンスを照合
2. 各 Agent が参照するスクリプト・references が目的関数と対応しているか確認
3. Agent 間で同一スクリプトを参照している箇所がないか確認（scan-metrics.py と scan-skills.py は別 Agent 担当）

---

### Cat-6: 統合ステップの情報損失

**定義**: 各 Agent の報告は詳細だが、Step 3 の統合時に「証拠なき指摘」として除外され、最終サマリが薄くなる。

#### 読むファイル

- `~/.claude/skills/claude-config/SKILL.md`（Step 3: 結果統合と改善提案）

#### 比較基準

- `docs/harness-engineering-domain-model.md` §4.2（Steering Loop: 捕捉→蓄積→昇格）

  ```
  知見源 → 捕捉(自動化) → 蓄積 → 昇格(人間レビュー)
  ```

  Step 3 は「捕捉→蓄積」の接続点。Agent 出力の知見が昇格候補として保持されること。

#### 典型症状

- 個々の Agent が詳細な指摘を出すが、最終の改善提案テーブルに残る項目数が少ない
- Agent 2 のメモリ昇華候補が改善提案テーブルに含まれない
- 「証拠不十分」として除外された指摘が次回診断で再び同じ形で登場する（知見が消えている）

#### 診断手順

1. SKILL.md Step 3 の「証拠なき指摘を除外」フィルタの基準を確認
2. domain-model.md §4.2 の Steering Loop と照合し、除外ロジックが「捕捉」を阻害していないか評価
3. 除外された指摘が次のステップ（tmp/review への書き出し等）に何らかの形で保存されているか確認

---

### Cat-7: 入力データの不足（hook レベル）

**定義**: スクリプトや Agent プロンプト自体の問題ではなく、hook が未設定または未稼働で計測データが収集されていない。

> **このカテゴリは harness-tuning のスコープ外。**
> hook 追加が必要な場合は `~/.dotfiles/.claude/rules/config-management.md` の「hook スクリプトを追加/移動するとき」ガイドを参照し、ユーザーと相談のうえ別途対応する。

#### 読むファイル

- `~/.dotfiles/.claude/rules/config-management.md` §2「変更ガイド: hook スクリプトを追加/移動するとき」
- `~/.dotfiles/.claude-global/settings.json` の hooks セクション（log-permission-request.sh の登録確認）

#### 比較基準

- `config-management.md` §1「強連動: settings.json command パス」— hook が settings.json に登録され実スクリプトが存在していること
- domain-model.md §6.1 — PermissionRequest hook 実装済みとして `permission-requests.jsonl` にデータが記録されていること

#### 典型症状

- `~/.claude/tmp/metrics/permission-requests.jsonl` が空またはファイルが存在しない
- scan-metrics.py の出力に `tool_counts` キーはあるが全カウントが 0
- domain-model.md §6.1（PermissionRequest hookログ）が「実装済み」だが実際には動作していない

#### 対処方針

hook レベルの不足は SKILL.md やスクリプトを修正しても解決しない。`/update-config` または手動での settings.json 編集が必要。harness-tuning から提案として報告し、実装はユーザー判断に委ねる。

---

### Cat-8: モード設計の問題

**定義**: 診断モード・自己改善モード・能動的探索モードのいずれかのモード定義、またはモード間フローに設計上の問題がある。SKILL.md の構造変更を伴う。

> **単独で修正しない。** `/harness-tuning` 呼び出し時にユーザーと議論してから対応する。

#### 読むファイル

- `~/.claude/skills/claude-config/SKILL.md`（Step 0: モード判定, 各モードのフロー全体）

#### 比較基準

- `docs/harness-engineering-domain-model.md` §4（3領域と Steering Loop）
  - 診断は「自己管理」領域。モード設計の変更は Steering Loop への影響を評価すること

#### 典型症状

- モード判定（Step 0 の引数分岐）が意図しないモードを選択する
- 診断モードと自己改善モードが同一フローで混在し、片方の品質が下がる
- 共通レビューフェイズ（R1-R3）が特定モードで機能しない

#### 対処方針

モード設計の変更は SKILL.md の大規模編集を伴い、コナセンス破綻リスクが高い。まず問題の影響範囲をユーザーと確認し、変更の優先度・方針を合意してから修正する。修正後はコナセンスチェックリスト全項目を確認する。
