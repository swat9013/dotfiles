---
name: drawio
description: draw.io XMLを生成し.drawioファイル保存、CLIでPNG/SVG/PDFエクスポート。Use when「drawio」「.drawio」「図を作って」。
model: sonnet
effort: medium
---

# draw.io Diagram Skill

draw.io XMLを生成し`.drawio`ファイルとして保存。CLIで`.drawio.svg`/`.drawio.png`/`.drawio.pdf`にエクスポート。

## ワークフロー

### 1. XML生成

ユーザーの要求に合ったmxGraphModel XMLを生成する。

基本構造:

```xml
<mxGraphModel adaptiveColors="auto">
  <root>
    <mxCell id="0"/>
    <mxCell id="1" parent="0"/>
    <!-- diagram cells here with parent="1" -->
  </root>
</mxGraphModel>
```

XML詳細は `references/xml-reference.md` を参照。

### 2. ファイル保存

Write toolで`.drawio`ファイルを保存。ファイル名はkebab-case、内容に基づく名前。

### 3. エクスポート

draw.io CLIでエクスポート:

```bash
/Applications/draw.io.app/Contents/MacOS/draw.io -x -f <format> -b 10 -o <name>.drawio.<ext> <name>.drawio
```

主要フラグ:
- `-x`: エクスポートモード
- `-f png|svg|pdf`: 出力フォーマット
- `-b 10`: ボーダー幅
- `-t`: 透過背景（PNGのみ）
- `-s <scale>`: スケール

> **Gotcha: `-e`（XML埋め込み）フラグ禁止** — v29.6.6 で PNG の `IEND` チャンクが欠損し破損ファイルになる。Claude API が `400 "Could not process image"` を返す原因。`.drawio` ソースファイルを保持すれば編集可能性は担保される。

エクスポート成功後、`.drawio`ソースファイルは保持する（XMLの正本）。

### 4. 結果表示

`open <file>` で開く。失敗時はファイルパスを表示。

## 出力フォーマット判定

| ユーザー入力例 | 出力 |
|--------------|------|
| `フローチャート作って` | `flowchart.drawio`（デフォルト） |
| `png フローチャート` | `flowchart.drawio.png` |
| `svg ER図` | `er-diagram.drawio.svg` |
| `pdf アーキテクチャ` | `architecture.drawio.pdf` |

フォーマット指定なしなら`.drawio`のみ保存。

## XML生成ルール

- **edge必須**: 全edge mxCellに`<mxGeometry relative="1" as="geometry" />`子要素。edge mxCell自体の自己閉じ禁止
- **ノード間隔**: 水平200px / 垂直120px以上
- **グリッド**: 10の倍数に配置
- **矢印余白**: ターゲット前に20px以上の直線セグメント
- **edgeラベル**: HTML不要（デフォルト11px）
- **ダークモード**: `adaptiveColors="auto"`で自動対応
- **XMLコメント**: `--`禁止（XML仕様違反）
- **コンテナ**: ネスト要素は`parent="containerId"`で親子関係を定義

詳細は `references/xml-reference.md` を参照。

## CLIが見つからない場合

`.drawio`ファイルをそのまま保持し、draw.ioデスクトップアプリのインストールを案内:

```
brew install --cask drawio
```
