# Tuning Guide Extended: Cat-5〜Cat-10

基礎パターン（Cat-1〜Cat-4）と修正チェックリストは [tuning-guide.md](tuning-guide.md) を参照。

## TOC

- [Cat-5: Agent 間の責務分割の問題](#cat-5-agent-間の責務分割の問題)
- [Cat-6: 統合ステップの情報損失](#cat-6-統合ステップの情報損失)
- [Cat-7: 入力データの不足（hook レベル）](#cat-7-入力データの不足hook-レベル)
- [Cat-8: モード設計の問題](#cat-8-モード設計の問題)
- [Cat-9: Computational First 違反（skill 内 LLM 処理の script 化余地）](#cat-9-computational-first-違反skill-内-llm-処理の-script-化余地)
- [Cat-10: rules の linter/formatter 化余地](#cat-10-rules-の-linterformatter-化余地)

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

---

### Cat-9: Computational First 違反（skill 内 LLM 処理の script 化余地）

**定義**: skill の Step 記述に「LLM 推論を必要としない決定論的処理」が LLM 手順として残っている。`~/.dotfiles/CLAUDE.md` の Harness Architecture 原則「Computational First — 計測可能な基準はスクリプト化する。判断が必要なものだけ推論に委ねる」に違反している状態。

#### 読むファイル

- 対象 skill の `SKILL.md`（診断対象として Step 1 で特定されたもの）
- `~/.dotfiles/.claude-global/skills/*/scripts/`（既存 script 類型の参考に）

#### 比較基準

- `~/.dotfiles/CLAUDE.md` Harness Architecture 原則「Computational First」
- `~/.dotfiles/docs/harness-engineering-domain-model.md` #architecture-fitness-harness 計算的制御4分類
  - B(検証) が「部分的」状態 → script 化により充実方向に進む対象

#### 決定論的処理の判定基準（LLM 不要と判断する目安）

| パターン | 例 | script 化手段 |
|---------|-----|------------|
| 数値カウント・集計 | 「N 個以上」「何回現れる」 | Python 集計スクリプト |
| 正規表現マッチ | 「〜形式の文字列を抽出」 | `grep -E` / Python `re` |
| 構造チェック | 「必須セクションが存在」「TOC とセクション一致」 | Markdown parser / 単純 grep |
| ファイル存在・パス整合性 | 「A が参照する B が存在」「symlink が切れていない」 | `test -e` / `readlink` |
| JSON/YAML 整形・妥当性 | 「schema に従う」「必須キー存在」 | `jq` / `python -c "import json"` |
| 重複・命名規約 | 「同名の定義が複数」「命名が規約通り」 | 集計スクリプト |

**script 化しない（LLM に残す）対象**: 意味の解釈、品質評価、設計判断、因果関係の推定。

#### 典型症状

- skill の Step に「〜であれば」「〜のときは」など決定論的分岐記述があるが script 化されていない
- skill 起動のたびに LLM が同じ決定論的処理を繰り返し実行している
- ユーザーが「この部分は毎回同じなのでスクリプトにしてほしい」と指摘している
- scripts/ ディレクトリが存在せず、SKILL.md の手順量が肥大化している

#### 診断手順

1. 対象 skill の SKILL.md を Read し、各 Step の記述を上記「決定論的処理の判定基準」と照合
2. 該当箇所ごとに script 化手段と期待効果（LLM トークン削減・実行速度・再現性）を記録
3. 既存の `scripts/` ディレクトリ有無・既存 script との統合可能性を確認
4. 優先度 = 実行頻度 × LLM トークン量で序列化

#### 対処方針

- 決定論的処理が 1 箇所なら SKILL.md 内の Bash コマンドとして埋め込む
- 複数箇所・再利用性があれば `scripts/` ディレクトリに切り出し、SKILL.md からは「このスクリプトを実行して結果を読む」形式へ書き換える
- script 追加時は `settings.json` の `permissions.allow` への登録と `chmod +x` を忘れない（コナセンス連動）

---

### Cat-10: rules の linter/formatter 化余地

**定義**: `~/.dotfiles/.claude/rules/*.md` や `~/.dotfiles/.claude-global/rules/*.md` に記述された「決定論的に検証可能な規則」が、LLM の解釈任せのまま linter/formatter（hook scripts）に落ちていない状態。Feedforward（推論的）のみで Feedback（計算的）が欠けている。

#### 読むファイル

- `~/.dotfiles/.claude/rules/*.md`（プロジェクト rule）
- `~/.dotfiles/.claude-global/rules/*.md`（グローバル rule）
- `~/.dotfiles/.claude-global/hooks/`（既存 hook scripts: guard-*.sh, ruff-check.sh 等）
- `~/.dotfiles/.claude-global/settings.json`（hooks セクション）

#### 比較基準

- `~/.dotfiles/CLAUDE.md` Harness Architecture 原則「Computational First」「Guide First, Sense Second」
- `~/.dotfiles/docs/harness-engineering-domain-model.md` #architecture-fitness-harness
  - Feedforward（推論的）と Feedback（計算的）の対比
  - 計算的制御4分類 A(ガード)/B(検証) は script 化により強化する対象
- 既存 hook 類型（guard-*.sh の禁止操作遮断、ruff-check.sh の PostToolUse lint）

#### lint/format 化可能性の判定基準

| rule 記述パターン | 決定論性 | 化しうる hook / lint |
|----------------|---------|-------------------|
| 「〜してはならない」「〜を禁止」 | 高 | PreToolUse guard-*.sh / permissions.deny |
| 「〜形式で書く」「必ず〜を含める」 | 中〜高 | PostToolUse formatter / structure-check |
| 「命名は〜規約」 | 高 | PostToolUse 命名 linter |
| 「リンクが切れていないか」「参照先が存在」 | 高 | Stop hook / SessionEnd hook で整合性チェック |
| 「〜と〜が同期している」（コナセンス） | 高 | 一貫性チェック script |
| 「〜を考慮する」「〜に留意」 | 低 | LLM に残す（解釈要） |

#### 典型症状

- rules に「〜してはならない」「必ず〜する」形式の規則があるが対応する hook が無い
- 同じ違反（同じ rule の破り方）が retrospective やユーザー指摘で繰り返し登場している
- PostToolUse hook は ruff のみで他言語・他種類の検証が無い
- rules が「LLM に伝えれば守られるはず」前提で肥大化し、context lot を圧迫している

#### 診断手順

1. 対象 rules ファイルを Read し、各規則を上記判定基準で分類（高=lint 化余地、中=検討、低=LLM に残す）
2. 「高」分類の規則ごとに、既存 hook scripts で類似実装があるか確認（流用可能性）
3. settings.json hooks セクションと照合し、未実装の hook event（Pre/PostToolUse, Stop, SessionEnd）を特定
4. context lot 削減効果 = rule 行数 × 該当規則の占有比率で優先度付け

#### 対処方針

- Cat-10 の修正は hook 追加を伴うため、**harness-tuning では提案のみ行い、実装は `/update-config` に委譲する**（Cat-7 と同構造）
- 既存 hook を拡張できる場合は修正箇所と手段を具体的に提示（「guard-rm.sh に `--force` パターンを追加」等）
- rule 自体の削除/縮小提案もセットで出す（hook 化後は rule 記述が冗長になるため）
- `config-management.md` のコナセンス「hook scripts ↔ settings.json ↔ rules 記述」を確認し、3 者が整合する形で提案する
