# Tuning Guide: claude-config 診断・修正リファレンス

claude-config コンポーネント（スクリプト / Agent プロンプト / references）の
診断時に参照する 8 分類パターンと、修正後のコナセンスチェックリスト。

---

## TOC

1. [原因分類テーブル（8 カテゴリ）](#1-原因分類テーブル)
2. [カテゴリ別診断パターン](#2-カテゴリ別診断パターン)
   - [Cat-1: スクリプトの出力不足](#cat-1-スクリプトの出力不足)
   - [Cat-2: スクリプトの出力精度](#cat-2-スクリプトの出力精度)
   - [Cat-3: Agent プロンプトの指示不足](#cat-3-agent-プロンプトの指示不足)
   - [Cat-4: references の基準不足](#cat-4-references-の基準不足)
   - [Cat-5: Agent 間の責務分割の問題](#cat-5-agent-間の責務分割の問題)
   - [Cat-6: 統合ステップの情報損失](#cat-6-統合ステップの情報損失)
   - [Cat-7: 入力データの不足（hook レベル）](#cat-7-入力データの不足hook-レベル)
   - [Cat-8: モード設計の問題](#cat-8-モード設計の問題)
3. [修正時のコナセンスチェックリスト](#3-修正時のコナセンスチェックリスト)

---

## 1. 原因分類テーブル

| # | カテゴリ | 修正対象 | スコープ |
|---|---------|---------|--------|
| Cat-1 | スクリプトの出力不足 | scan-metrics.py, scan-skills.py | 計測対象の網羅性 |
| Cat-2 | スクリプトの出力精度 | scan スクリプト群 | パターン抽出・リスク判定ロジック |
| Cat-3 | Agent プロンプトの指示不足 | SKILL.md の Agent テーブル, diagnostic-agent-prompt.md | 目的関数・品質基準の明確さ |
| Cat-4 | references の基準不足 | references/ 配下（skills.md, permissions.md 等） | 評価軸の網羅性 |
| Cat-5 | Agent 間の責務分割の問題 | SKILL.md の Agent テーブル全体 | 守備範囲の境界定義 |
| Cat-6 | 統合ステップの情報損失 | SKILL.md Step 3（結果統合） | 知見の捕捉→蓄積→昇格フロー |
| Cat-7 | 入力データの不足（hook レベル） | **スコープ外**（hook 追加が必要） | PermissionRequest ログ等の収集基盤 |
| Cat-8 | モード設計の問題 | SKILL.md 構造変更 | モード定義・フロー全体 |

---

## 2. カテゴリ別診断パターン

---

### Cat-1: スクリプトの出力不足

**定義**: 集計ロジックが計測対象を網羅していない。本来カウントすべき指標がレポートに現れない。

#### 読むファイル

- `~/.dotfiles/.claude-global/skills/claude-config/scripts/scan-metrics.py`
- `~/.dotfiles/.claude-global/skills/claude-config/scripts/scan-skills.py`

#### 比較基準

- `docs/harness-engineering-domain-model.md` §5（計測対象 ①②③）
  - ①: ツール呼び出しパターン（Bash 拒否率、guard ブロック、PermissionRequest 回数）
  - ②: コンテキスト消費（注入 rules/skills 本数・総行数、サブエージェント起動数）
  - ③: 品質指標（lint 失敗率、ユーザーのツール拒否回数）
- 同 §6.1-6.3（オープン課題の方針）

#### 典型症状

- 診断レポートに `tool_counts` が空または一部ツールのみに偏る
- 「PermissionRequest が多い」という定性指摘はあるが、ツール別の具体的カウントが出ない
- Agent 1 が「自動化の穴」の specific な特定に至らず、汎用提案のみ出す

#### 診断手順

1. scan-metrics.py の集計キー一覧と domain-model.md §5 計測対象①②③を並べて照合
2. 欠落している計測対象をリストアップ
3. スクリプト内の集計ロジック（ループ・JSON キー参照）が欠落対象をスキップしていることを確認

---

### Cat-2: スクリプトの出力精度

**定義**: 集計対象は網羅されているが、パターン抽出・リスク判定のロジックが不適切で、誤判定または過小判定が生じる。

#### 読むファイル

- `~/.dotfiles/.claude-global/skills/claude-config/scripts/scan-metrics.py`
- `~/.dotfiles/.claude-global/skills/claude-config/scripts/scan-hooks.py`
- `~/.dotfiles/.claude-global/skills/claude-config/scripts/scan-skills.py`

#### 比較基準

- `docs/harness-engineering-domain-model.md` §5.2（2段構えの実装）
  - Tier 1: JSONL 解析（定性データ）
  - Tier 2: hook ログ解析（PermissionRequest 等の定量データ）
  - 2つの Tier が混在しないことが前提

#### 典型症状

- Bash による危険操作が Low リスク判定される（閾値または正規表現が甘い）
- allow 追加の提案が毎回同じパターンで繰り返される（一時的なスパイクと慢性的な頻出を区別できていない）
- is_error フラグの guard ブロック検出が機能せず、guard ブロック回数が 0 として集計される

#### 診断手順

1. スクリプトのリスク判定ロジック（条件分岐・閾値）を読み、誤判定の原因箇所を特定
2. domain-model.md §5.2 の Tier 定義と照合し、Tier 境界をまたいだ集計が起きていないか確認
3. テストケースとして known-bad なパターンを入力し、期待する判定と実際の判定を比較

---

### Cat-3: Agent プロンプトの指示不足

**定義**: Agent の目的関数の記述が曖昧、または品質基準の記述が欠落しており、事実→結論の直結指摘しか出ない。

#### 読むファイル

- `~/.claude/skills/claude-config/SKILL.md`（Step 2 の Agent テーブル）
- `~/.claude/skills/claude-config/references/diagnostic-agent-prompt.md`

#### 比較基準

- `references/diagnostic-agent-prompt.md` の「浅い分析と深い分析の違い」テーブル

  | 浅い分析（NG） | 深い分析（OK） |
  |---------------|---------------|
  | 「PermissionRequest が多い → allow 追加」 | 「Bash(npm:*) が 60%。settings.md リスク評価 Low。allow で承認 12回/日→0回」 |
  | 「未使用スキル → 削除」 | 「skill X: 14日間 0回、複雑度 refs×2+scripts×1。評価期間不足の可能性」 |

- 同ファイルの成功基準（因果関係・証拠・根拠の3要件）

#### 典型症状

- 「〜を確認してください」など事実→結論の直結指摘が多い
- ファイルパス・行番号・スクリプト出力の具体値を持たない指摘が混在する
- references のどの基準に基づくか不明な指摘が含まれる

#### 診断手順

1. SKILL.md の Agent テーブルの「目的関数」列を確認し、目的関数が因果関係を含む形で記述されているか評価
2. diagnostic-agent-prompt.md のテンプレート（`{purpose_function}` プレースホルダ）と SKILL.md の記述を照合
3. 直近の診断レポートを参照可能であれば、指摘の3要件（因果関係・証拠・根拠）の充足率を確認

---

### Cat-4: references の基準不足

**定義**: references に記載された判断基準に評価軸が欠落しており、Agent が「基準外だが懸念あり」として分離報告せざるを得ない指摘が多発する。

#### 読むファイル

- `~/.claude/skills/claude-config/references/skills.md`
- `~/.claude/skills/claude-config/references/hooks.md`
- `~/.claude/skills/claude-config/references/settings.md`（permissions.md が存在する場合はそちらも）

#### 比較基準

- `references/diagnostic-agent-prompt.md` の成功基準（因果関係・証拠・根拠の3要件）
  - 「根拠: references のどの基準に基づくか明記」が充足できない = references の基準不足

#### 典型症状

- 診断レポートの「基準外だが懸念あり」セクションの指摘数が、通常指摘数を超える
- 同じ「懸念あり」パターンが複数回の診断でも基準化されていない
- Agent が「リスク評価の根拠を示せない」と付記する

#### 診断手順

1. 直近の診断レポートで「基準外だが懸念あり」として分離された指摘を収集
2. 各指摘が references のどのファイルのどのセクションに追記されるべきかを特定
3. 追記後、次回診断で同パターンが正式基準に基づく指摘として分類されることを確認

---

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

---

## 3. 修正時のコナセンスチェックリスト

修正した分類に対応する項目を確認する。`~/.dotfiles/.claude/rules/config-management.md` のコナセンス・マップが正式ソース。

### スクリプト変更時（Cat-1, Cat-2）

- [ ] `SKILL.md` の Step 2 Agent テーブルの「実行スクリプト」列と実ファイルパスが一致しているか
- [ ] config-management.md の「scan スクリプト分割」コナセンス行に変更内容を反映したか
- [ ] 新しい集計キーを追加した場合、該当 Agent の `references` がその指標を評価できる基準を持っているか（Cat-4 に波及していないか）
- [ ] scan-metrics.py と scan-skills.py の集計対象が重複していないか（前者: tool_usage 全体+permissions、後者: skill_usage のみ）

### Agent プロンプト変更時（Cat-3, Cat-5）

- [ ] 変更した Agent の目的関数が `diagnostic-agent-prompt.md` のテンプレートに沿った記述になっているか
- [ ] 目的関数が「因果関係を含む形式」（「〜が〜により損なわれる」）で記述されているか
- [ ] Agent 間の守備範囲境界が config-management.md の「scan スクリプト分割」コナセンスと整合しているか
- [ ] 変更した Agent が参照するスクリプトとその Agent の目的関数が対応しているか

### references 変更時（Cat-4）

- [ ] 追記した基準が `skill-design-patterns.md` または `skills.md` と重複していないか
- [ ] 両ファイルに同内容が存在する場合、正規ソース（`rules/claude-global-skills.md`）との整合性を確認したか
- [ ] 追記後、次回診断で同パターンの指摘が「基準外」ではなく正式指摘として分類されることを確認したか

### 統合ステップ変更時（Cat-6）

- [ ] 「証拠なき指摘の除外」フィルタが Steering Loop の「捕捉」フェーズを阻害していないか
- [ ] Agent 2 のメモリ昇華候補が最終改善提案テーブルに必ず含まれるフローになっているか
- [ ] 除外された指摘が `tmp/review/` への書き出し等で保存されているか

### 共通（すべての修正後）

- [ ] `settings.json` の `permissions.allow` にスクリプトパスが登録されているか（スクリプト追加時）
- [ ] スクリプトを追加/移動した場合、実行権限（`chmod +x`）を付与したか
- [ ] ドキュメント参照パスは `~/.claude/skills/...`（シンボリックリンク経由）、スクリプト実行パスは `~/.dotfiles/.claude-global/...`（実ファイルパス）の規則に従っているか
