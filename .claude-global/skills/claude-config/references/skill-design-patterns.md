# スキル設計パターン

## Contents
- [Progressive Disclosure](#progressive-disclosure)
- [処理の3層分離](#処理の3層分離)
  - [言語選択基準](#言語選択基準)
  - [Python スクリプト共通構造](#python-スクリプト共通構造)
- [自由度パターン](#自由度パターン)
- [Setupパターン](#setupパターン)
- [設計注意事項](#設計注意事項)
- [アンチパターン](#アンチパターン)
- [肥大化パターン（P1-P6）](#肥大化パターンp1-p6)
- [圧縮テクニック](#圧縮テクニック)
- [バージョン追従パターン](#バージョン追従パターン)

---

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

---

## 処理の3層分離

スキル内の処理を性質で分け、適切な実行方法に割り当てる。

| 層 | 性質 | 実行方法 | 例 |
|----|------|---------|-----|
| 機械的実行 | 入出力が確定的 | スクリプト/Bash直接 | git diff、ファイル一覧取得 |
| 機械的分析 | ルールベースの判定 | スクリプト | lint実行、ツール検出、リスク判定 |
| AI判断 | 文脈依存の判断 | サブエージェント | レビュー、メッセージ生成、設計判断 |

### 判断基準

「この処理の出力は入力が同じなら常に同じか？」→ Yes ならスクリプト化の候補。

### スクリプト設計の要点

- **終端判定キー**: quality-gate.py は JSON `gate` フィールド (`PASS`/`FAIL`/`SKIP`)、changed-files.sh は `RESULT: NO_CHANGES/PROCEED` 等のテキストキーを出力。SKILL.mdが判定キーで分岐
- **配置**: スキル固有→`scripts/`、複数スキル共有→`skills/scripts/`

### 言語選択基準

| 判定 | 条件 |
|------|------|
| **シェル維持** | CLIコマンドの薄い合成（パイプ・`&&`チェーン中心、ロジック最小） |
| **Python移行** | ロジック含む処理（JSON解析、パターンマッチ、条件分岐が多い、50行超の関数） |

判断の問い: 「このスクリプトはコマンドをつなぐだけか、それともデータを変換・判定しているか？」

### Python スクリプト共通構造

詳細は `script-patterns.md` を参照。

---

## 自由度パターン

詳細は `script-patterns.md` を参照。

---

## Setupパターン

ユーザー固有設定（Slackチャンネル、APIキー等）が必要なスキルは、初回実行時に AskUserQuestion で確認し `${CLAUDE_PLUGIN_DATA}/config.json` に保存する。スキルディレクトリ内のデータはアップグレードで消えるため、`${CLAUDE_PLUGIN_DATA}` を使用。

---

## 設計注意事項

- **スキルリネーム時の同時更新箇所**: ディレクトリ名・frontmatter name・タイトル・使用例コマンド名・スキルカタログ（claude-global-skills.md）の5箇所
- **`[NEEDS CLARIFICATION]` マーカー**: 非対話的成果物向け。対話型では直接質問。知識系は特定コンテキスト→既存`references/`内包、複数→独立スキル
- **references に書かない内容**: 一般知識の列挙は価値低。プロジェクト固有の判断基準のみ。ハードコード日付は腐るので相対表現を使う
- **調査→スキル化後**: 元の report.md 等を削除するか参照に更新。放置すると二重管理
- **`$ARGUMENTS` 展開の注意**: 長い引き継ぎ文をそのまま渡すと TaskCreate の subject が肥大化。要約して渡す
- **手順記述スタイル**: 番号リストと`####`見出しの混在を避けフラットなリスト構造に統一。BSD/GNU互換性（macOS awk/sed差異）確認必須
- **Railroading回避**: 情報と目的を与え手順の柔軟性を保つ。過度に具体的な手順指定はClaudeの適応力を制限する
- **model/effort 設定**: ワークフロー系スキルのみ（知識系=`user-invocable:false`への設定は親セッション設定上書きのため禁止）。model: `opus`→高精度判断（architect等）、`sonnet`→標準。effort: `high`→複雑推論、`medium`→標準、`low`→定型処理

---

## アンチパターン

`rules/claude-global-skills.md` アンチパターン表の項目（500行超、15,000文字超、TOCなし、トリガーなしdescription、汎用description、深い参照ネスト）は除外。以下はそれ以外の独自項目。

### 選択肢の提示しすぎ

デフォルトを1つ示し、代替はエスケープハッチとして提示する。

```markdown
# 悪い例
pypdf、pdfplumber、PyMuPDF、pdf2imageのいずれかを使用...

# 良い例
テキスト抽出にはpdfplumberを使用。
スキャンPDFでOCRが必要な場合のみpdf2image + pytesseractを使用。
```

### 用語の不統一

同じ概念に複数の用語を使わない。

```markdown
# 悪い例: 混在
「APIエンドポイント」「URL」「APIルート」「パス」

# 良い例: 統一
常に「APIエンドポイント」
```

---

## 肥大化パターン（P1-P6）

スキルが肥大化する典型的パターン。早期検出して対処する。

| ID | パターン名 | 症状 | 対処 |
|----|-----------|------|------|
| P1 | サブエージェントプロンプトのインライン展開 | SKILL.md の 20-25% がサブエージェント向けテンプレート | references/ に分離、または簡潔な指示に圧縮 |
| P2 | TaskCreate/TaskUpdate ボイラープレート重複 | 全ワークフロースキルが各15-30行の呼び出しパターンを個別記述 | _shared/ に共通パターンを集約 |
| P3 | Claude が既知の概念の説明 | MECE、リファクタリング原則等の一般知識を記述 | 削除（Claude は既に知っている） |
| P4 | 「役割」セクションが description と重複 | スキル冒頭の役割説明が frontmatter description と同一内容 | 役割セクション削除、description に一本化 |
| P5 | 網羅的な NG 例・除外リスト | 「報告不要」「除外」リストが各10-20行 | 最小限に絞る（3項目以下） |
| P6 | 出力フォーマットの詳細テンプレートをインライン | テーブル定義・出力形式が本文に埋め込み | references/ に分離 |

---

## 圧縮テクニック

肥大化パターンへの具体的な圧縮手法。

| テクニック | 適用条件 | 削減見込み |
|-----------|---------|----------|
| description と重複する「役割」セクション削除 | 全スキル | 各3-5行 |
| Claude が既知の概念の説明削除 | architect(MECE), refactor(Fowler引用) | 各5-15行 |
| 「報告不要」「除外」リストの最小化 | architect, claude-config | 各10-20行 |
| 冗長な例を1つに絞る | dialogue, contextual-commits | 各5-10行 |
| 説明文→表/dense notation への変換 | 全スキル | 各10-20% |

---

## バージョン追従パターン

詳細は `version-tracking.md` を参照。
