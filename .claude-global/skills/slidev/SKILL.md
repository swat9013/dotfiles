---
name: slidev
description: |-
  Slidev（Markdownベースプレゼンテーション）のスライド内容生成ガイド。
  レイアウト・テーマ・アニメーション・コードハイライト・フォント設定の構文仕様を含む。
  「Slidev」「スライド作成」「プレゼン作成」「sli.dev」と依頼された時に参照する。
user-invocable: false
---

# Slidev スライド内容生成ガイド

## 基本構造

スライドは `---` で区切る。各スライドの先頭に YAML frontmatter を置ける。

```md
---
layout: cover
---

# タイトル

サブタイトル

---

# 2枚目のスライド

本文

<!-- プレゼンターノート。聴衆には見えない -->

---
layout: center
---

# 3枚目
```

## 作成の流れ

1. デッキ frontmatter 設定（テーマ・フォント・メタ情報）
2. スライド構成を決める（1スライド1メッセージ）
3. 各スライドにレイアウトを選択
4. アニメーションを要所に追加（控えめに）

## frontmatter テンプレート

### デッキレベル（先頭スライド）

```yaml
---
theme: seriph
title: プレゼンタイトル
fonts:
  sans: 'Noto Sans JP'
  mono: 'JetBrains Mono'
  weights: '200,400,600,700'
  local: 'Noto Sans JP'   # PDF出力時に必須
transition: slide-left
---
```

### スライドレベル

```yaml
---
layout: image-right
image: /path/to/image.jpg
transition: fade
hideInToc: true
---
```

## レイアウト選択ガイド

| レイアウト | 用途 | こんなスライドに使う |
|-----------|------|-------------------|
| `cover` | タイトルスライド | 発表タイトル・登壇者名 |
| `section` | セクション区切り | 章の切り替え |
| `default` | 標準 | 箇条書き・説明文 |
| `two-cols` | 左右2列 | 比較・Before/After |
| `two-cols-header` | ヘッダー + 2列 | 見出し付き比較 |
| `image-right` / `image-left` | 片側画像 | スクショ + 説明、図解 + 解説 |
| `fact` | 数字強調 | KPI・統計値を1つ大きく見せる |
| `quote` | 引用強調 | 格言・ユーザーの声・キーメッセージ |
| `center` | 中央配置 | 一言メッセージ・問いかけ |
| `end` | 最終スライド | Thank you・QA |

他に `full`, `none`, `intro`, `statement`, `image`, `iframe`, `iframe-left`, `iframe-right` がある。

## テーマ選択ガイド

```yaml
---
theme: seriph   # slidev-theme- プレフィックス省略可
---
```

| テーマ | 特徴 | 向いている用途 |
|--------|------|---------------|
| **seriph** | エレガント・タイポグラフィ重視 | カンファレンス・技術発表 |
| **default** | クリーン・シンプル | 汎用 |
| **apple-basic** | Apple風ミニマル | プロダクト発表 |
| **bricks** | モダン・グリッドデザイン | デザイン系 |
| **penguin** | カジュアル・2/3レイアウト対応 | 勉強会・コミュニティ |
| **neversink** | フラット・明るい原色 | 学術発表・講義 |

## デザインチェックリスト

1. **コンテンツを絞る** — 1スライド1メッセージ、箇条書き3〜5項目
2. **フォントサイズ** — タイトル40pt+、本文24pt+、最低18pt。1スライド最大3種類
3. **レイアウトを選ぶ** — `fact`・`quote`・`two-cols`・`image-right` を積極活用
4. **色を3色に絞る** — メインカラー + アクセント + ベース（白/黒/グレー）
5. **余白を確保する** — UnoCSS の `p-8`, `m-4`, `gap-6` で一貫性
6. **アニメーションは控えめに** — `v-clicks` と Magic Move は要所だけ
7. **視覚的階層** — サイズ・太さ・色の3つで表現。1要素につき1つの変化

## 頻出構文

### クリックアニメーション

```html
<v-clicks>

- アイテム1
- アイテム2
- アイテム3

</v-clicks>
```

`v-click="3"` で絶対位置指定、`v-click.hide` でクリック後非表示、`v-after` で前のv-clickと同時表示。

### レイアウトスロット

```md
---
layout: two-cols
---

# 左列タイトル

左列コンテンツ

::right::

# 右列タイトル

右列コンテンツ
```

`two-cols-header` は `::left::` と `::right::` の両方を使う。

### コードハイライト

行ハイライト: ` ```ts {2,3} ` 。クリック切替: ` ```ts {2-3|5|all} `

### Shiki Magic Move

`````md
````md magic-move
```js
console.log(`Step ${1}`)
```
```js
console.log(`Step ${1 + 1}`)
```
````
`````

### トランジション

frontmatter に `transition: slide-left`
組み込み: `fade`, `fade-out`, `slide-left`, `slide-right`, `slide-up`, `slide-down`
前進・後退で別指定: `slide-left | slide-right`

### プレゼンターノート

```md
# スライドタイトル

コンテンツ

<!-- これはプレゼンターノート。聴衆には表示されない -->
```

### その他の構文

Monaco Editor、v-motion、LaTeX数式、Mermaidダイアグラム、画像配置、マルチファイル構成:
[snippets.md](references/snippets.md)

## Gotchas

- **画像パス**: `public/` に配置して絶対パス `/image.png` で参照。相対パスはビルド後に壊れる
- **スコープドCSS**: 子孫結合子セレクタ（`.a > .b`）は使用不可
- **Shiki Magic Move**: トランスフォーマー非対応
- **PDFエクスポート**: Monaco等のインタラクティブ機能は失われる

詳細な問題と対策: [troubleshooting.md](references/troubleshooting.md)
