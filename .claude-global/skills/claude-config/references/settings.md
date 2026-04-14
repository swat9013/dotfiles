# Permissions リスク評価リファレンス

Agent 1 が permissions.allow ルール提案時に参照する判断基準。

## 1. Permissions モデル概要

**評価優先度**: `deny（管理スコープ）> deny > deny pattern > allow > ask`

- **deny** はフォールバック安全ネット: hooks がバイパスされた場合（Task tool 経由等）でも操作をブロックする
- **allow** を追加するときは deny との矛盾がないか必ず確認する

**二層防御の役割分担**

| 層 | 仕組み | 役割 |
|----|--------|------|
| 第1層 | `guard-*.sh`（PreToolUse hook） | カスタムメッセージ付き禁止・ログ記録 |
| 第2層 | `permissions.deny` | hooks バイパス時のフォールバック |

同一操作は両層に設定する。一方だけでは防御が不完全。

## 2. Allow ルールパターンとセキュリティ表面積

| パターン | 例 | セキュリティ表面積 |
|---------|-----|-------------------|
| ツール全体 | `Bash` | 最大（全コマンド許可） |
| プレフィックス指定 | `Bash(git:*)` | コマンドのサブコマンド全体 |
| パス指定 | `Write(~/.dotfiles/**)` | 指定パス配下の全ファイル |
| 完全一致 | `Bash(npm test:*)` | 最小 |

## 3. リスク評価フレームワーク

`scan-metrics.py` の `suggestions[].risk` フィールドと対応する。提案前に以下の基準で評価する。

| リスクレベル | 条件 | 例 |
|------------|------|-----|
| **high** | 任意コード実行可能・ファイル削除/上書き・認証情報アクセス | `rm`, `chmod`, `curl（任意URL）`, `.env`パス |
| **medium** | 外部通信・git 履歴変更・パッケージインストール | `git rebase`, `npm install`, `curl`（固定URL） |
| **low** | ローカル読み取り・ビルド・テスト実行 | `npm test`, `rg`, `wc` |

**注意**: `scan-metrics.py` の `assess_risk()` はコマンド名の前方一致で機械的に判定する。ワイルドカードで展開される操作範囲はスクリプトが評価しないため、Agent 1 が補完評価する。

## 4. ワイルドカードスコープ分析の指針

プレフィックスが「安全な操作のみに制限する」のに十分かを評価する。

**例: `Bash(uv:*)` の場合**

| サブコマンド | 展開される操作 | リスク |
|------------|--------------|--------|
| `uv run` | 任意の Python スクリプトを実行 | high |
| `uv pip install` | 任意パッケージをインストール | high |
| `uv run --with <pkg>` | 一時パッケージを取得して実行 | high |

→ **`Bash(uv:*)` は high リスク。サブコマンド単位で分割すること**: `Bash(uv pip list:*)`, `Bash(uv sync:*)` 等

**一般原則**:
1. `*` の前のプレフィックスが「書き込み/実行/通信のない操作のみ」に制限できるか確認する
2. 制限できない場合は medium 以上に格上げし、緩和策を提示する
3. ツール全体（`Bash` のみ）の allow は原則推奨しない

## 5. 提案テンプレート

Agent 1 が allow ルール提案時に含める要素:

```
- ルール: Bash(uv pip list:*)
- リスク: low
- 表面積: インストール済みパッケージ一覧表示のみ。書き込み・外部通信なし
- 緩和策: （low のため不要）
```

medium 以上の場合は必ず緩和策を添える:

```
- ルール: Bash(npm install:*)
- リスク: medium
- 表面積: 任意パッケージのインストール。package.json 変更を伴う
- 緩和策: Bash(npm install --save-dev:*) に絞る、または guard-shell.sh でパッケージ名の明示を要求
```

## 6. 二層防御との整合チェック

allow を追加する前に確認する:

1. 同一操作が `permissions.deny` に設定されていないか（矛盾）
2. 対応する `guard-*.sh` のカバー範囲と重複しないか（guard なしの空白地帯にならないか）
3. Task tool（サブエージェント）経由での実行時も hooks がバイパスされる前提で評価したか

## 7. Allow 無効パターンの診断

`scan-metrics.py` の `allow_ineffective` キーに出力されるデータ。allow ルールが存在するにもかかわらず PermissionRequest が発生しているパターンを示す。

**前提**: PermissionRequest ログに記録されている = Claude Code が自動許可しなかった。`matches_allow_rule()` の判定と実際の Claude Code 動作が乖離している。

### 原因分類

| 原因 | 説明 | 確認方法 |
|------|------|---------|
| compound command | `&&`/`||`/`;` で結合されたコマンド。Claude Code が prefix マッチを適用しない可能性 | `allow_ineffective` の Bash パターンで compound 比率が高いか |
| settings scope | グローバル allow がプロジェクトコンテキストで適用されない可能性 | PermissionRequest の `cwd` が特定プロジェクトに偏っているか |
| サブエージェント | Task/Agent tool 経由で permission 設定が継承されない可能性 | PermissionRequest がサブエージェント起動後のタイミングに集中しているか |
| パターン仕様変更 | Claude Code バージョンアップで prefix マッチ `cmd:*` の挙動が変わった可能性 | 広範なツール（Glob, Read 等のツール全体 allow 含む）が一様に出現するか |

### Agent 1 の判断フロー

1. `allow_ineffective` に 2回以上出現するパターンを確認
2. 上記原因分類のいずれに該当するか判定
3. 該当原因に応じて改善案を提示:
   - compound command → スキル/スクリプト側でコマンド分割を推奨
   - settings scope → プロジェクト settings.json の有無を確認、allow ルールの配置先を提案
   - サブエージェント → `allowed-tools` frontmatter や Task 起動時の permission 設定を確認
   - パターン仕様変更 → `/claude-config update auto` で仕様差分を調査
4. 広範なツール（Glob, Read, Bash の基本コマンド群）が一様に出現する場合はシステミックな問題として報告し、個別対応ではなく根本原因の調査を推奨
