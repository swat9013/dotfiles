# draw.io XML Reference

## TOC

1. [Common styles](#common-styles)
2. [Style properties](#style-properties)
3. [Edge routing](#edge-routing)
4. [Containers and groups](#containers-and-groups)
5. [Dark mode](#dark-mode)

## Common styles

**Rounded rectangle:**
```xml
<mxCell id="2" value="Label" style="rounded=1;whiteSpace=wrap;" vertex="1" parent="1">
  <mxGeometry x="100" y="100" width="120" height="60" as="geometry"/>
</mxCell>
```

**Diamond (decision):**
```xml
<mxCell id="3" value="Condition?" style="rhombus;whiteSpace=wrap;" vertex="1" parent="1">
  <mxGeometry x="100" y="200" width="120" height="80" as="geometry"/>
</mxCell>
```

**Cylinder (database):**
```xml
<mxCell id="4" value="DB" style="shape=cylinder3;whiteSpace=wrap;" vertex="1" parent="1">
  <mxGeometry x="100" y="300" width="120" height="80" as="geometry"/>
</mxCell>
```

**Edge (arrow):**
```xml
<mxCell id="e1" value="" style="edgeStyle=orthogonalEdgeStyle;" edge="1" source="2" target="3" parent="1">
  <mxGeometry relative="1" as="geometry"/>
</mxCell>
```

**Labeled edge:**
```xml
<mxCell id="e2" value="Yes" style="edgeStyle=orthogonalEdgeStyle;" edge="1" source="3" target="4" parent="1">
  <mxGeometry relative="1" as="geometry"/>
</mxCell>
```

## Style properties

| Property | Values | Use |
|----------|--------|-----|
| `rounded=1` | 0/1 | 角丸 |
| `whiteSpace=wrap` | wrap | テキスト折り返し |
| `fillColor=#dae8fc` | Hex | 背景色 |
| `strokeColor=#6c8ebf` | Hex | 枠線色 |
| `fontColor=#333333` | Hex | 文字色 |
| `shape=cylinder3` | shape名 | DBシリンダー |
| `ellipse` | keyword | 円/楕円 |
| `rhombus` | keyword | ダイヤモンド |
| `edgeStyle=orthogonalEdgeStyle` | keyword | 直角コネクタ |
| `edgeStyle=elbowEdgeStyle` | keyword | エルボーコネクタ |
| `dashed=1` | 0/1 | 破線 |
| `swimlane` | keyword | スイムレーンコンテナ |
| `group` | keyword | 不可視コンテナ |
| `container=1` | 0/1 | コンテナ化 |
| `pointerEvents=0` | 0/1 | 接続キャプチャ防止 |

## Edge routing

**必須**: edge mxCellは自己閉じにせず、子要素`<mxGeometry>`を含める。edge mxCell自体を自己閉じにすると描画されない:

```xml
<!-- GOOD -->
<mxCell id="e1" edge="1" parent="1" source="a" target="b" style="edgeStyle=orthogonalEdgeStyle;">
  <mxGeometry relative="1" as="geometry" />
</mxCell>

<!-- BAD - renders nothing -->
<mxCell id="e1" edge="1" parent="1" source="a" target="b" style="edgeStyle=orthogonalEdgeStyle;" />
```

**Waypoints** (エッジが重なる場合):
```xml
<mxCell id="e1" style="edgeStyle=orthogonalEdgeStyle;" edge="1" parent="1" source="a" target="b">
  <mxGeometry relative="1" as="geometry">
    <Array as="points">
      <mxPoint x="300" y="150"/>
      <mxPoint x="300" y="250"/>
    </Array>
  </mxGeometry>
</mxCell>
```

接続ポイント制御: `exitX`/`exitY`/`entryX`/`entryY` (0-1) でノードのどの辺から接続するか指定。

## Containers and groups

ネスト要素は`parent="containerId"`で親子関係を定義。子は**コンテナ内の相対座標**を使用。

### Container types

| Type | Style | 用途 |
|------|-------|------|
| Group | `group;` | 不可視。接続なし。`pointerEvents=0`含む |
| Swimlane | `swimlane;startSize=30;` | タイトルバー付き。コンテナ自体の接続OK |
| Custom | `container=1;pointerEvents=0;` | 任意シェイプをコンテナ化 |

**Swimlane example:**
```xml
<mxCell id="svc1" value="User Service" style="swimlane;startSize=30;fillColor=#dae8fc;strokeColor=#6c8ebf;" vertex="1" parent="1">
  <mxGeometry x="100" y="100" width="300" height="200" as="geometry"/>
</mxCell>
<mxCell id="api1" value="REST API" style="rounded=1;whiteSpace=wrap;" vertex="1" parent="svc1">
  <mxGeometry x="20" y="40" width="120" height="60" as="geometry"/>
</mxCell>
```

## Dark mode

`adaptiveColors="auto"`をmxGraphModelに設定すると自動対応。

- デフォルト色（`default`）: ライト=黒、ダーク=白
- 明示色（`fillColor=#DAE8FC`）: ライトモード色。ダークモードは自動反転
- 両方指定: `fontColor=light-dark(#7EA6E0,#FF0000)`

通常は自動反転で十分。`light-dark()`は反転結果が不適切な場合のみ使用。
