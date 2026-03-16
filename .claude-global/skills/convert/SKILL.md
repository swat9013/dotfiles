---
name: convert
description: Figma URL から HTML/CSS を自動生成するパイプライン。MCP でデザイン取得 → セクション分割 → AI 変換 → Playwright 検証 → AI ループ仕上げ。「/convert」「Figma変換」「HTML生成」「パイプライン実行」「figma2html」と依頼された時に使用。
user-invocable: true
---

# /convert コマンド

Figma デザインから WordPress 組み込み可能な HTML/CSS を自動生成する。

## 前提条件

- `.env` に `FIGMA_API_KEY`、`FIGMA_FILE_KEY` が設定済み
- `prompts/transform.md` が存在する
- `scripts/` 配下のユーティリティスクリプトが存在する

## 引数

```
/convert --sp-node <node-id> --pc-node <node-id>
```

引数なしの場合は AskUserQuestion で SP/PC の node-id を確認する。

## パイプライン概要

```
① fetch → ② transform → ③ validate → ④ fix loop → ⑤ output
```

| ステージ | 実行方法 | 出力先 |
|---------|---------|--------|
| ① fetch | Figma MCP ツール直接呼び出し | `stage1/` |
| ② transform | Claude 自身が JSX → HTML/CSS 変換 | `stage2/` |
| ③ validate | scripts/ を Bash 実行 | `stage3/` |
| ④ fix loop | Claude 自身が CSS 修正 | `stage2/` → `stage3/` |
| ⑤ output | ファイルコピー | `output/` |

## 実行手順

### ① fetch: Figma データ取得

1. `stage1/` ディレクトリを作成
2. Figma MCP `get_design_context` を **2 回**呼び出し（SP + PC）
   - `nodeId`: 引数の node-id
   - `fileKey`: `.env` の `FIGMA_FILE_KEY`
3. 取得した JSX を `stage1/sp.jsx`、`stage1/pc.jsx` に保存
4. 取得したスクリーンショットを `stage1/sp.png`、`stage1/pc.png` に保存
5. `scripts/figma-api.mjs` で `absoluteBoundingBox` 座標データを取得 → `stage1/coordinates.json` に保存

**MCP rate limit 時**: `scripts/figma-api.mjs` で REST API フォールバック。

### ② transform: AI 変換（品質の主レバー）

1. `stage1/sp.jsx` と `stage1/pc.jsx` を読み込む
2. `data-node-id` 属性でセクション境界を特定し、6 セクションに分割
   - 分割失敗時は全体を 1 セクションとしてフォールバック
3. `prompts/transform.md` を読み込む
4. `stage1/coordinates.json` の座標データを参照し、**セクションごとに** HTML/CSS を生成:
   - absolute → flexbox/grid 変換
   - grid/table 要素のみ `min-height`（`absoluteBoundingBox.height`）設定
   - flex gap = 次要素 y - 現要素 y（inter-element y-distance）
   - PC + SP → `@media (min-width: 768px)` 統合
5. セクション結合 → `stage2/index.html` + `stage2/style.css` に Write

**重要**: transform の品質がパイプライン全体を決定する。PoC4-5 で AI ループは +0.99%/3 回が上限と確定済み。

### ③ validate: 品質検証

以下を **順次** Bash 実行:

```bash
node scripts/screenshot.mjs --input stage2/index.html --output stage3/sp-actual.png --width 375 --height 812
node scripts/screenshot.mjs --input stage2/index.html --output stage3/pc-actual.png --width 1280 --height 800
node scripts/measure-dom.mjs --input stage2/index.html --width 375 --output stage3/sections.json
node scripts/compare-sections.mjs --expected stage1/sp.png --actual stage3/sp-actual.png --sections stage3/sections.json --output stage3/sp-sections.json
node scripts/compare.mjs --expected stage1/sp.png --actual stage3/sp-actual.png --diff stage3/sp-diff.png --output stage3/sp-overall.json
node scripts/lint-absolute.mjs stage2/style.css
node scripts/lint-css.mjs stage2/style.css
```

結果を `stage3/report.json` に統合して Write。スキーマ: [schemas.md](references/schemas.md)

### ④ fix loop: AI CSS 修正

**進入条件**: `report.json` の verdict が FAIL

**ループ処理**:
1. `stage3/sp-diff.png` を Read（diff 画像を視覚的に確認）
2. `prompts/fix.md` を読み込む
3. `stage3/report.json` のセクション別スコア + `stage1/coordinates.json` の座標データを参照
4. **CSS のみ修正**（HTML 構造は固定）して `stage2/style.css` を Edit
5. ③ validate を再実行
6. `report.json` を更新

**停止条件（3 重安全弁）**:
- 最大 **5 回**
- 改善率 < **0.3%** が **3 回連続**
- タイムアウト **7 分**（パイプライン開始から）

**CSS 修正の禁止事項**:
- spacing 変更（padding/margin/gap/height）禁止 — PoC5 で逆相関確定
- 修正対象: 色、画像サイズ、フォントサイズ、visibility、min-height（grid/table のみ）

### ⑤ output: 最終成果物

1. `stage2/index.html` + `stage2/style.css` を `output/` にコピー
2. verdict が FAIL のまま停止した場合は警告メッセージを出力

## M1 合格基準

| 指標 | SP | PC |
|------|----|----|
| セクション加重 matchRate | 78%+ | 87%+ |
| 全ページ matchRate（参考） | 75%+ | 85%+ |
| SSIM | 0.35+ | 0.35+ |
| absolute 使用率 | <10% | <10% |

## 出力形式

```
## パイプライン完了

**verdict**: PASS / FAIL（警告付き）
**イテレーション数**: X 回
**セクション加重 matchRate**: SP XX.X% / PC XX.X%
**全ページ matchRate（参考）**: SP XX.X% / PC XX.X%
**absolute 使用率**: X.X%
**所要時間**: Xm Xs

**出力先**: output/index.html, output/style.css
```

## 設計制約（PoC 確定事項）

詳細: [constraints.md](references/constraints.md)

| ID | 制約 | 対策 |
|----|------|------|
| C1 | Figma テキストフレーム固定高さ | min-height は grid/table のみ。セクション単位比較 |
| C2 | CSS spacing 変更と matchRate 逆相関 | fix loop で spacing 変更禁止 |
| C3 | AI CSS-only ループ上限 +0.99%/3回 | transform 品質が最重要。ループは仕上げ |
