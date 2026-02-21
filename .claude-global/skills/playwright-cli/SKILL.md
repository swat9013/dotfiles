---
name: playwright-cli
description: Playwright CLIのコマンドリファレンス。テスト実行、コード生成、デバッグ、レポート、ブラウザ管理の操作方法を含む。「Playwright」「playwright test」「E2Eテスト実行」「テストコード生成」「codegen」「トレース」「playwright設定」「テストレポート」と依頼された時に参照する。
user-invocable: false
---

# Playwright CLI

## agent-browser との使い分け

| 観点 | playwright-cli | agent-browser |
|------|---------------|---------------|
| 用途 | テスト作成・実行・CI | アドホックなブラウザ操作・確認 |
| 実行形態 | テストファイル (.spec.ts) を書いて実行 | 対話的にコマンド発行 |
| 選択基準 | 再現可能なテストスイートが必要 | 一度きりの操作・スクレイピング・画面確認 |

## パッケージマネージャー

プロジェクトの lockfile から判断する:

| lockfile | コマンド |
|----------|---------|
| package-lock.json | `npx playwright` |
| pnpm-lock.yaml | `pnpm exec playwright` |
| yarn.lock | `yarn playwright` |
| bun.lockb | `bunx playwright` |

以降の例は `npx` で記載。プロジェクトに合わせて読み替えること。

## Core workflow

1. **codegen** → ブラウザ操作を記録してテストコードの骨格を生成
2. **edit** → アサーション追加、ロケーター改善、テスト構造化（codegen出力はそのまま使わない）
3. **test** → 実行（開発中は `--ui` 推奨）
4. **debug** → 失敗時のツール選択は下記 Debugging 参照
5. **report** → `show-report` で結果確認

## Debugging（迷いやすいポイント）

| 状況 | ツール | コマンド |
|------|--------|---------|
| 開発中にインタラクティブに試行錯誤 | UI Mode | `--ui` |
| 特定テストをステップ実行したい | Inspector | `--debug` |
| CI で落ちたテストを事後分析 | Trace Viewer | `show-trace <path>` |
| ロケーターが正しいか確認したい | Inspector | `--debug` で Pick locator |

### トレース記録モードの選択

| モード | いつ使うか |
|--------|-----------|
| `on-first-retry` | **CI標準**。パフォーマンスと診断情報のバランスが最良 |
| `retain-on-failure` | リトライなしで失敗時のみトレースが欲しいとき |
| `on` | 全テスト記録。デバッグ目的の一時的な使用のみ |

`trace.playwright.dev` でブラウザ上からも閲覧可能（インストール不要）。

## ARIA Snapshot（ページ構造の確認）

ページ構造の確認にはスクリーンショットよりARIAスナップショットを優先する。
アクセシビリティツリーを構造化テキストで取得でき、AIエージェントにとって解析しやすい。

| 場面 | 利点 |
|------|------|
| ページ構造の理解 | DOM全体を読まずにセマンティック構造を把握 |
| 要素階層の検証 | 親子関係をツリー形式で確認 |
| ロケーター問題のデバッグ | 要素の role/name を直接確認 |

```typescript
// 取得
const snapshot = await page.locator('body').ariaSnapshot();

// アサーション
await expect(page.locator('body')).toMatchAriaSnapshot(`
  - navigation "Main":
    - link "Home"
    - link "About"
  - main:
    - heading "Welcome" [level=1]
`);
```

スクリーンショットはビジュアルリグレッション検出用。構造的な検証にはARIAスナップショットを使う。

## CI/CD で間違えやすい設定

```typescript
// playwright.config.ts — CI で必須の設定
export default defineConfig({
  forbidOnly: !!process.env.CI,     // test.only の混入防止
  retries: process.env.CI ? 2 : 0,  // CI のみリトライ
  workers: process.env.CI ? 1 : undefined,  // CI は安定性優先
  use: {
    trace: 'on-first-retry',          // CI標準
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },
});
```

### GitHub Actions アーティファクト

```yaml
- uses: actions/upload-artifact@v4
  if: failure()
  with:
    name: playwright-report
    path: playwright-report/
```

### シャーディング

`--shard=N/M` で分散。`--reporter=blob` で出力し `merge-reports` で結合。
`fullyParallel: true` が前提（ファイル単位だと偏る）。

## codegen の非自明なオプション

```bash
# 認証フローの記録 → 保存 → 別テストで再利用
npx playwright codegen --save-storage=auth.json https://example.com/login
npx playwright codegen --load-storage=auth.json https://example.com/dashboard

# モバイル + ダークモード + 日本語でのcodegen
npx playwright codegen --device="iPhone 13" --color-scheme=dark --lang="ja-JP" <url>
```

認証状態ファイル（auth.json）は `.gitignore` に追加すること。

## Config で迷う設定

基本configはClaudeの既知情報。非自明なパターン: `references/config-recipes.md`

特に注意が必要な設定:
- **認証状態の共有**: setup project + `dependencies` + `storageState` の3点セット
- **タイムアウト階層**: test > action > expect > navigation の優先順位を理解する
- **webServer**: `reuseExistingServer: !process.env.CI` でローカル/CI両対応
