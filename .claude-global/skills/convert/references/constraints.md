# 設計制約（PoC1-5 確定事項）

## C1: テキストフレーム固定高さの非互換性

Figma デザイナーはテキストフレームを CSS auto-height より大きく固定設定する（例: Hero title フレーム 125px、実テキスト 57px）。この「空白パディング」はレイアウト意図の一部だが、CSS flex-stack では再現不可能。

- **影響**: 全ページ matchRate が真の品質を約 20pt 過小評価する
- **対策**:
  - min-height は grid/table 要素のみ適用
  - flex-stack 要素（ヒーロー、テキストブロック等）には min-height 禁止
  - セクション単位比較を主要指標に採用
- **検証**: PoC5 T3-A で確定。flex-stack に min-height 適用 → catastrophic 63.51%

## C2: CSS spacing 変更と matchRate の逆相関

flex-stacked 要素の spacing（padding/margin/gap）を変更すると、テキストリフローが発生し新たなピクセル差が生まれ、matchRate が悪化する。

- **影響**: fix loop での spacing 修正が品質を劣化させる
- **対策**: `prompts/fix.md` で spacing 変更を禁止。修正対象は色、画像サイズ、フォントサイズ、visibility に限定
- **検証**: PoC5 T2 で確定（ドリフト-12% → matchRate -1.82%）

## C3: AI CSS-only ループの上限

transform 後の CSS-only ループは +0.99%/3 回が実測上限。当初仮定の「AI ループが品質改善の主要レバー」は否定された。

- **影響**: ループに過度な期待を持つと時間を浪費する
- **対策**: 初回 transform プロンプト品質が最大のレバー。AI ループは最大 5 回の「仕上げ」
- **検証**: PoC4 T2 で確定

## その他の確定事項

| 項目 | 結果 | 検証元 |
|------|------|--------|
| フォント hinting | 効果ゼロ（±0.00%） | PoC4 T3 |
| `data-node-id` 属性名 | MCP 出力は `data-node-id`（`data-figma-node-id` ではない） | PoC4 T1 |
| パイプライン時間 | 5-6 分（10 分以内） | PoC4 |
| 要素単位品質 | ~90%（全ページ 71% は位置ドリフトで過小評価） | PoC5 T3-B |
| absolute→flexbox 変換 | 全テストで 0 件達成 | PoC1-3 |
| display:none 禁止プロンプト | 無効（10 件のまま） | PoC2 |
