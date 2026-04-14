# Tuning Guide: claude-config 診断・修正リファレンス

claude-config コンポーネント（スクリプト / Agent プロンプト / references）の
診断時に参照する 8 分類パターンと、修正後のコナセンスチェックリスト。

---

## TOC

1. [原因分類テーブル（10 カテゴリ）](#1-原因分類テーブル)
2. [カテゴリ別診断パターン](#2-カテゴリ別診断パターン)
   - Cat-1〜Cat-4: 本ファイル
   - Cat-5〜Cat-10: [tuning-guide-extended.md](tuning-guide-extended.md)
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
| Cat-9 | Computational First 違反（skill 内 LLM 処理の script 化余地） | 対象 skill の SKILL.md / `scripts/` | LLM 手順に残る決定論的処理 |
| Cat-10 | rules の linter/formatter 化余地 | rules/*.md / hook scripts / settings.json | Feedforward 依存の決定論的規則 |

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

> **Cat-5〜Cat-10**: [tuning-guide-extended.md](tuning-guide-extended.md) を参照

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

### skill 内処理の script 化時（Cat-9）

- [ ] 切り出した script が対象 skill の `scripts/` 配下に配置されているか
- [ ] `settings.json` の `permissions.allow` に実行パスが登録されているか
- [ ] `chmod +x` で実行権限が付与されているか
- [ ] SKILL.md から新 script を呼び出す記述に差し替え、元の決定論的手順を削除したか
- [ ] LLM に残す判断部分（意味解釈・設計判断）との境界が明確か

### rules の linter/formatter 化時（Cat-10）

- [ ] **実装は `/update-config` に委譲**。harness-tuning では提案テーブル生成で完結させる
- [ ] 提案に hook event（Pre/PostToolUse/Stop/SessionEnd）と対象 matcher が具体的に記載されているか
- [ ] 既存 hook scripts（guard-*.sh, ruff-check.sh 等）の拡張可能性を評価したか
- [ ] hook 化後の rule 記述の削除/縮小提案が併せて出されているか（context lot 削減）
- [ ] `config-management.md` の「hook scripts ↔ settings.json ↔ rules 記述」コナセンスと整合するか

### 共通（すべての修正後）

- [ ] `settings.json` の `permissions.allow` にスクリプトパスが登録されているか（スクリプト追加時）
- [ ] スクリプトを追加/移動した場合、実行権限（`chmod +x`）を付与したか
- [ ] ドキュメント参照パスは `~/.claude/skills/...`（シンボリックリンク経由）、スクリプト実行パスは `~/.dotfiles/.claude-global/...`（実ファイルパス）の規則に従っているか
