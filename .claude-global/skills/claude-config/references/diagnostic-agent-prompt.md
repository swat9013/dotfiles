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
