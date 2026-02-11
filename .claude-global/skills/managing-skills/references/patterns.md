# スキル設計パターン

## Progressive Disclosure

コンテキストウィンドウは共有資源。段階的に情報を開示する。

### 3層構造

| 層 | 内容 | 読み込みタイミング |
|----|------|-------------------|
| frontmatter | name + description | 常時（~100語） |
| SKILL.md本文 | 手順・ガイドライン | スキル起動時 |
| references | 詳細ドキュメント | 必要時のみ |

### 分離の判断基準

**SKILL.md本文に含める:**
- 必ず実行する手順
- 基本的なガイドライン
- 簡潔な例

**referencesに分離:**
- APIリファレンス
- 詳細なスキーマ定義
- 長い例・バリエーション
- ドメイン固有の知識

### 構成パターン

#### パターン1: 高レベルガイド + references

最も一般的。SKILL.mdに概要とクイックスタート、詳細はreferencesへ。

```markdown
# PDF Processing

## Quick start
[基本的な使い方を3-5行で]

## Advanced features
**Form filling**: See [FORMS.md](FORMS.md)
**API reference**: See [REFERENCE.md](REFERENCE.md)
```

#### パターン2: ドメイン別整理

複数ドメインを扱うスキル向け。ユーザーの質問に関連するファイルだけ読み込む。

```
bigquery-skill/
├── SKILL.md (概要 + ナビゲーション)
└── reference/
    ├── finance.md (収益、請求)
    ├── sales.md (パイプライン)
    └── product.md (利用状況)
```

#### パターン3: 条件付き詳細

基本は本文で完結、特定機能のみreferencesへ。

```markdown
## ドキュメント編集
単純な編集はXMLを直接変更。

**変更追跡が必要な場合**: See [REDLINING.md](REDLINING.md)
**OOXML仕様の詳細**: See [OOXML.md](OOXML.md)
```

### 100行超のreferencesファイル

冒頭に目次を設置。Claudeが部分読みしても全体構造を把握できる。

```markdown
# API Reference

## Contents
- Authentication and setup
- Core methods (create, read, update, delete)
- Error handling patterns
- Code examples

## Authentication and setup
...
```

## 自由度パターン

### 高自由度（テキストベースの指示）

複数アプローチが有効な場合。Claudeの判断に委ねる。

```markdown
## コードレビュー
1. コード構造と設計を分析
2. バグやエッジケースを検出
3. 可読性・保守性の改善を提案
4. プロジェクト慣習への準拠を確認
```

### 中自由度（パラメータ付きテンプレート）

推奨パターンがあり、カスタマイズも許容する場合。

````markdown
## レポート生成

以下のテンプレートを基に、必要に応じてカスタマイズ:

```python
def generate_report(data, format="markdown", include_charts=True):
    # データ処理
    # 指定フォーマットで出力
    # オプションで可視化を含む
```
````

### 低自由度（具体的スクリプト）

操作が壊れやすく、一貫性が重要な場合。

````markdown
## データベース移行

以下を正確に実行:

```bash
python scripts/migrate.py --verify --backup
```

コマンドの変更やフラグの追加は禁止。
````

## ワークフローパターン

### チェックリスト付きワークフロー

複雑な多段階タスクで進捗を追跡:

````markdown
## フォーム入力ワークフロー

チェックリストをコピーして進捗を追跡:

```
- [ ] Step 1: フォームを解析 (analyze_form.py)
- [ ] Step 2: フィールドマッピング作成 (fields.json)
- [ ] Step 3: マッピング検証 (validate_fields.py)
- [ ] Step 4: フォーム入力 (fill_form.py)
- [ ] Step 5: 出力検証 (verify_output.py)
```
````

### フィードバックループ

検証→修正→再検証のサイクルで品質を確保:

```markdown
## 編集プロセス

1. document.xmlを編集
2. **即座に検証**: python scripts/validate.py dir/
3. 検証失敗時:
   - エラーメッセージを確認
   - 問題を修正
   - 再度検証
4. **検証通過後のみ**次のステップへ
```

## 出力パターン

厳格な形式が必要な場合はテンプレートを提示。柔軟な場合は入出力ペアの例示で期待を伝える:

```markdown
## コミットメッセージの例

入力: ユーザー認証にJWTトークンを追加
出力:
feat(auth): implement JWT-based authentication

Add login endpoint and token validation middleware
```

## リソース選択ガイド

| リソース種別 | 用途 | 例 |
|-------------|------|-----|
| scripts/ | 反復実行するコード | validate.py, convert.sh |
| references/ | 参照ドキュメント | api_docs.md, schema.json |
| assets/ | 出力用ファイル | template.pptx, logo.png |

## アンチパターン

避けるべき設計:

1. **冗長な説明**: Claudeは賢い。既知の情報を繰り返さない
2. **深いネスト**: A→B→C の参照チェーンは避ける。SKILL.mdから1階層まで
3. **巨大なSKILL.md**: 500行超えたらreferencesに分離
4. **曖昧なdescription**: トリガーキーワードを明示的に含める
5. **選択肢の提示しすぎ**: デフォルトを1つ示し、代替はエスケープハッチとして提示

```markdown
# 悪い例
pypdf、pdfplumber、PyMuPDF、pdf2imageのいずれかを使用...

# 良い例
テキスト抽出にはpdfplumberを使用。
スキャンPDFでOCRが必要な場合のみpdf2image + pytesseractを使用。
```

6. **用語の不統一**: 同じ概念に複数の用語を使わない

```markdown
# 悪い例: 混在
「APIエンドポイント」「URL」「APIルート」「パス」

# 良い例: 統一
常に「APIエンドポイント」
```
