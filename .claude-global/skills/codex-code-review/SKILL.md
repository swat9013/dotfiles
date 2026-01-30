---
name: codex-code-review
description: Cursor Agent (GPT-5.2-Codex) によるアーキテクチャ・設計レビュー。「codexレビュー」「アーキテクチャレビュー」「設計レビュー」と依頼された時に使用。4観点（Architecture、Test Strategy、API Design、Behavior）で評価。
disable-model-invocation: true
---

# Codex Code Review

Cursor Agent CLI経由でGPT-5.2-Codexを使用し、高レベル設計レビューを実行するスキル。

## 観点

| 観点 | フォーカス |
|------|-----------|
| Architecture | レイヤー分離、依存方向、モジュール境界 |
| Test Strategy | テスト戦略、カバレッジ、テストケース設計 |
| API Design | API仕様、契約、一貫性 |
| Behavior | アプリ振る舞い、ユースケース整合性 |

## モデル選択

| 対象 | モデル |
|------|--------|
| コード（*.ts, *.js, *.py, etc.） | gpt-5.2-codex |
| ドキュメント（*.md, docs/**, rules/**） | gpt-5.2 |

## 前提条件

1. `cursor agent` コマンドが利用可能
2. レビュー対象ファイルが存在

## セキュリティ設定

**デフォルト**: `--sandbox enabled` でワークスペース内のみアクセス許可、ネットワーク遮断

| フラグ | 効果 |
|-------|------|
| `--sandbox enabled` | セキュアモード（ワークスペース内のみ、ネットワーク遮断） |
| `-f, --force` | コマンド自動許可（sandboxと併用可） |

## 実行手順

### Step 1: 対象特定と分類

ユーザーからレビュー対象を確認し、ファイルを分類:

- **コード**: ソースコード、設定ファイル（*.ts, *.js, *.py, *.go, *.json, etc.）
- **ドキュメント**: Markdown、docs/, rules/, README等

### Step 2: レビュー実行

対象の種類に応じて実行:

| ケース | 実行方法 |
|--------|---------|
| コードのみ | GPT-5.2-Codexで1回実行 |
| ドキュメントのみ | GPT-5.2で1回実行 |
| 両方混在 | 2つを**並列実行**（下記参照） |

#### 並列実行の例（混在時）

```bash
# コード用とドキュメント用を同時実行（sandbox有効化）
cursor agent --print "..." --sandbox enabled --model gpt-5.2-codex --output-format json > /tmp/codex-code.json &
cursor agent --print "..." --sandbox enabled --model gpt-5.2 --output-format json > /tmp/codex-doc.json &
wait
```

**オプション**: コマンド自動許可が必要な場合は `-f` を追加

#### コードレビュー用プロンプト
```bash
cursor agent --print "あなたはシニアソフトウェアアーキテクトです。以下の4観点でコードレビューを実施してください。

## 観点

### 1. Architecture
- レイヤー分離（Presentation/Application/Domain/Infrastructure）
- 依存方向（外から内へ、逆方向依存の検出）
- モジュール境界の明確さ
- 循環依存の有無

### 2. Test Strategy
- テストピラミッドの適切さ
- テストケースの網羅性（正常系、異常系、境界値）
- テストの独立性と再現性
- モック/スタブの適切な使用

### 3. API Design
- RESTful原則の遵守
- 一貫した命名規則
- エラーレスポンス設計
- 契約の明確さ（型定義）

### 4. Behavior
- ユースケースとの整合性
- ビジネスルールの正確な実装
- 状態遷移の正当性
- エッジケースの処理

## 高信号フィルタ

フラグすべき:
- レイヤー違反、循環依存
- 重要パスのテスト欠如
- 破壊的API変更
- ビジネスルール違反

フラグしない:
- スタイル好み
- 軽微な命名問題
- 将来の懸念

## レビュー対象ファイル
以下のファイルを読み取ってレビューしてください:
${ファイルパスリスト}

## 出力形式（JSON）
{
  \"architecture\": [{\"file\": \"path\", \"line\": N, \"problem\": \"...\", \"suggestion\": \"...\"}],
  \"test_strategy\": [...],
  \"api_design\": [...],
  \"behavior\": [...]
}" --sandbox enabled --model gpt-5.2-codex --output-format json
```

#### ドキュメントレビュー用プロンプト
```bash
cursor agent --print "あなたはテクニカルライティングとアーキテクチャドキュメントの専門家です。以下の4観点でドキュメントレビューを実施してください。

## 観点

### 1. Architecture
- 設計意図の明確さ
- コンポーネント間関係の説明
- 制約・前提条件の記載

### 2. Test Strategy
- テスト方針の明確さ
- カバレッジ目標の妥当性
- テストシナリオの網羅性

### 3. API Design
- API仕様の完全性
- リクエスト/レスポンス例の充実
- エラーケースの説明

### 4. Behavior
- ユースケースの明確さ
- シーケンス・フローの説明
- 境界条件の記載

## 高信号フィルタ

フラグすべき:
- 実装との不整合
- 重要情報の欠落
- 曖昧な記述
- 古い情報

フラグしない:
- 文体の好み
- フォーマットの軽微な問題

## レビュー対象ファイル
以下のファイルを読み取ってレビューしてください:
${ファイルパスリスト}

## 出力形式（JSON）
{
  \"architecture\": [{\"file\": \"path\", \"line\": N, \"problem\": \"...\", \"suggestion\": \"...\"}],
  \"test_strategy\": [...],
  \"api_design\": [...],
  \"behavior\": [...]
}" --sandbox enabled --model gpt-5.2 --output-format json
```

### Step 3: 結果統合

各agentの出力（JSON）を収集し、以下を実行:

1. **JSONパース**: 出力ファイルを読み取り
   ```bash
   cat /tmp/codex-code.json /tmp/codex-doc.json
   ```

2. **重複排除**: 同一ファイル・行の指摘をマージ

3. **観点別に整理**: architecture, test_strategy, api_design, behavior に分類

4. **優先度付け**: Architecture > API > Test > Behavior の順で並べ替え

### Step 4: 出力

`templates/output.md` の形式に従い、**カレントディレクトリに `codex-review.md` として書き出す**。

テンプレートのプレースホルダー（`{{TARGET}}`, `{{ARCH_ISSUES}}` 等）を実際の値で置換する。

## 成功基準

1. 高レベル設計問題のみを報告（低レベルコード品質は `/code-review` へ）
2. 具体的な修正案を含む
3. アーキテクチャ決定の根拠を示す
