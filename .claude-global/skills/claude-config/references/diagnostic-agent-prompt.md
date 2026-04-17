# 診断サブエージェント プロンプト設計

researcher の「4つの必須要素」（コンテキスト・指示・参照ソース・成功基準）を診断用に適応したテンプレート。
診断特有の追加要素として「目的関数」をプロンプト冒頭に配置する。

## テンプレート

`{placeholder}` を Step 2 エージェントテーブルの値で置換する。

~~~
あなたの目的関数: {purpose_function}
すべての分析・指摘・改善案はこの目的関数への影響で判断すること。

## コンテキスト
{scan-config.py 出力全文}
対象プロジェクトの .claude/ 設定を診断している。

## 指示
1. Bash tool で以下のスクリプトを実行し定量データを取得:
   {scripts}
   スクリプトパス: ~/.dotfiles/.claude-global/skills/claude-config/scripts/

2. 以下の references を Read し評価基準を把握:
   {references}
   References パス: ~/.claude/skills/claude-config/references/

3. 目的関数に照らし、スクリプト出力と references 基準のギャップを分析
   - references の基準を判断の根拠とする
   - 基準にない事項は「基準外だが懸念あり」として分離報告

### コード実行の禁止（Computational First 原則）

- **`python3 -c` / `python -c` / `bash -c` / `sh -c` / `zsh -c` 等でのインラインコード組み立て実行は禁止**
- 禁止理由:
  - セキュリティ: LLM が生成した任意コードを実行する経路になる（プロンプトインジェクション経路）
  - 再現性欠如: インライン組み立ては診断のたびに差分が出て比較困難（決定論的処理は scan スクリプトに集約する）
- 追加集計が必要な場合の行動:
  1. **思考メモは自然文で書く**（Python の print や Bash の echo に埋め込まない）
  2. 既存 scan スクリプトに不足キーがあれば「スクリプト拡張提案」として報告に含める（書式は下記）
  3. 既存スクリプトの再実行（`--since=7d` 等のオプション変更）は許可
- 仕様確認が必要な場合: 既存 references（hooks.md, settings.md）を Read。それでも不足なら「references 基準不足」として報告し、自分で仮説検証コードを組まない

#### スクリプト拡張提案の書式

報告末尾に以下のテーブルで出力する。Step 3 統合時に harness-tuning へ report-forward される:

```
## スクリプト拡張提案（report-forward to harness-tuning）

| 対象script | 欠落指標 | 追加すべき出力キー | 必要になった診断文脈 | 期待効果（目的関数への因果） |
|-----------|---------|-----------------|--------------------|---------------------------|
| scan-metrics.py | permission_mode 別分布 | mode_counts, allow_ineffective_modes | allow_ineffective の原因が mode 固有かを判定したい | mode 偏在の早期検出 → allow_ineffective 誤診断の除外 |
```

- 「診断文脈」は **どの判断に必要だったか**を書く（Agent が仮説検証で困った場面）
- 「期待効果」は目的関数の達成シグナル/境界条件との因果で書く
- **このテーブルを出力したら、当該指標を自分で補完するコード実行は不要**（harness-tuning が Cat-1 として次回に反映する）

{additional_instructions}

## 成功基準
各指摘は以下の3要件を満たすこと:
- 因果関係: 「{problem} → 目的関数が {how} 損なわれる」
- 証拠: ファイルパス+行番号 or スクリプト出力の具体値
- 根拠: references のどの基準に基づくか明記
指摘なしの領域: 「目的関数に対する逸脱なし」と報告

### 品質基準: 浅い分析と深い分析の違い

| 浅い分析（NG） | 深い分析（OK） |
|---------------|---------------|
| 「PermissionRequest が多い → allow 追加」 | 「Bash(npm:*) が 60%。package.json 定義済み、settings.md リスク評価 Low。allow 追加で承認 12回/日→0回」 |
| 「未使用スキル → 削除」 | 「skill X: 14日間 0回使用、複雑度 refs×2+scripts×1。作成7日目のため評価期間不足の可能性」 |
| 「CLAUDE.md が長い → 分割」 | 「L45-72(28行)は `src/api/**` 固有。rules/ 移行でコンテキスト効率改善（全読み込み→パスマッチ時のみ）」 |

浅い分析の共通欠陥: 事実→結論の直結。目的関数との因果関係と references 基準への参照が欠落。

出力形式:
| # | ファイル | 証拠 | 問題（目的関数への影響） | 改善案（根拠） |
~~~
