# スキーマ定義

## report.json

```json
{
  "iteration": 2,
  "timestamp": "2026-03-16T12:00:00Z",
  "sectionWeightedMatchRate": 72.2,
  "overallMatchRate": 71.81,
  "ssim": 0.2891,
  "heightDiffRatio": 0.0032,
  "sections": [
    {
      "name": "Header",
      "matchRate": 96.4,
      "ssim": 0.89,
      "drift": 0,
      "verdict": "PASS"
    }
  ],
  "absoluteUsageRate": 0.0,
  "verdict": "FAIL"
}
```

### セクション verdict 閾値

| verdict | matchRate | 意味 |
|---------|-----------|------|
| PASS | >= 95% | 修正不要 |
| WARNING | >= 90% | 軽微な修正で対応可能 |
| FAIL | < 90% | 重点修正対象 |

### 全体 verdict 閾値（diffRatio ベース）

| verdict | diffRatio | 意味 |
|---------|-----------|------|
| PASS | < 5% | 十分な品質 |
| WARNING | 5-10% | 手修正で対応可能 |
| FAIL | >= 10% | 根本的改善が必要 |

## ステージ出力ディレクトリ

| ディレクトリ | 内容 | ライフサイクル |
|-------------|------|--------------|
| `stage1/` | MCP 生出力: `pc.jsx`, `sp.jsx`, `pc.png`, `sp.png`, `coordinates.json` | fetch 時に上書き。以降は読み取り専用 |
| `stage2/` | 変換済み: `index.html`, `style.css` | transform 時に上書き。fix loop で CSS のみ更新 |
| `stage3/` | 検証結果: `report.json`, `sections.json`, `sp-diff.png`, `pc-diff.png`, `sp-actual.png`, `pc-actual.png`, `sp-sections.json`, `sp-overall.json` | validate 時に上書き |
| `output/` | 最終成果物: `index.html`, `style.css` | 検証合格時にコピー |

## coordinates.json

```json
{
  "nodes": {
    "1:158": {
      "name": "SP Frame",
      "absoluteBoundingBox": { "x": 0, "y": 0, "width": 375, "height": 5000 },
      "children": [
        {
          "id": "1:159",
          "name": "Header",
          "absoluteBoundingBox": { "x": 0, "y": 0, "width": 375, "height": 60 },
          "absoluteRenderBounds": { "x": 0, "y": 0, "width": 375, "height": 60 }
        }
      ]
    }
  }
}
```
