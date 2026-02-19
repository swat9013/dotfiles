# Slidev トラブルシューティング

## PDFエクスポート

### 主要オプション

```bash
slidev export \
  --format pdf \
  --with-clicks \          # クリックを別ページとして出力
  --range 1,3-5,10 \       # 特定スライドのみ
  --dark \                  # ダークテーマで出力
  --timeout 60000 \         # タイムアウト延長
  --wait 5000 \             # 描画待機
  --with-toc               # PDF目次を生成
```

### 既知の問題と対策

| 問題 | 解決策 |
|------|--------|
| タイムアウトエラー | `--timeout 60000` で延長 |
| 日本語フォントが化ける | `fonts.local` にフォント名を明示指定 |
| 絵文字が消える | `noto-fonts-emoji` をシステムにインストール |
| アイコンがPDFに含まれない | PNGに変換するかSPAエクスポート使用 |
| 動画・iframeが含まれない | PDFでは不可。SPAエクスポートを代替 |

### 前提条件

- CLIエクスポートには `playwright-chromium` が必要
- インタラクティブ機能（Monaco等）はPDFで失われる

## フォント関連

### 日本語フォントの正しい設定

```yaml
fonts:
  sans: 'Noto Sans JP'
  local: 'Noto Sans JP'   # fonts.sans と fonts.local の両方指定が必要
```

- `sans` のみだとCDN経由のWebフォントのみ使用 → PDF出力で文字化けリスク
- `local` を追加するとシステムフォントも参照 → PDF出力が安定
- `provider: none` でCDNを無効化（オフライン環境向け）

## 画像・メディア

- `public/` に配置して絶対パス `/image.png` で参照（推奨）
- フロントマターやコンポーネント内の相対パスはビルド後に壊れる可能性あり
- 背景画像もフロントマターで `background: /image.png` と絶対パス指定

## CSS・スタイリング

### スコープドCSSの制限

- 子孫結合子セレクタ（`.a > .b`）は使用不可
- スライドごとにスコープされるため、グローバルに影響しない

### UnoCSS統合プリセット（デフォルト有効）

| プリセット | 内容 |
|-----------|------|
| `@unocss/preset-wind3` | Tailwind/Windi CSS互換 |
| `@unocss/preset-attributify` | 属性モード（`text="red xl"`） |
| `@unocss/preset-icons` | アイコンのクラス化 |
| `@unocss/preset-web-fonts` | Webフォント |
| `@unocss/transformer-directives` | `@apply` ディレクティブ |

## セットアップ参考

### ディレクトリ構成

```
your-slidev/
├── public/              # 画像（絶対パスで参照）
├── styles/index.css     # グローバルスタイル
├── components/
│   ├── global-top.vue   # 全スライドの上部レイヤー
│   └── global-bottom.vue # フッター（ページ番号等）
├── layouts/             # カスタムレイアウト
└── slides.md
```

### グローバルフッター（global-bottom.vue）

```vue
<template>
  <footer class="absolute bottom-4 right-4 text-sm opacity-50">
    {{ $slidev.configs.title }} - {{ $nav.currentPage }} / {{ $nav.total }}
  </footer>
</template>
```

### UnoCSS カラーパレット定義

```ts
// uno.config.ts
export default defineConfig({
  shortcuts: {
    'bg-main': 'bg-white text-[#181818] dark:(bg-[#121212] text-[#ddd])',
    'text-accent': 'text-blue-600 dark:text-blue-400',
  },
})
```

### Vite設定

- **vite.config.ts でSlidev内部プラグインを再追加しない** — 重大な落とし穴
- スライドデータ依存のViteプラグインは `setup/vite-plugins.ts` で追加（v51.5.0+）

### ドローイング機能

```yaml
drawings:
  enabled: true
  persist: true        # .slidev/drawings/ にSVGとして保存
  presenterOnly: true  # プレゼンターのみ描画可能
```

`persist: true` でPDFエクスポートにも描画が含まれる。

## バージョン関連

v51.0.0（2025年1月）から Epoch SemVer 方式に移行。`0.x` → `51` への意図的な命名変更。
主要な破壊的変更: Prism完全削除（Shiki一本化）、Shiki v2 移行。
