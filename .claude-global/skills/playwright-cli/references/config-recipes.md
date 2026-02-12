# Playwright Config Recipes

Claudeが既に知っている基本config（projects, use, reporter）は省略。
ここでは**組み合わせが非自明なパターン**のみ記載。

## 認証状態の共有（setup project パターン）

3つの要素が揃って初めて動作する:

```typescript
// playwright.config.ts
projects: [
  // 1. setup project: 認証を実行して状態を保存
  {
    name: 'setup',
    testMatch: /.*\.setup\.ts/,
  },
  // 2. dependencies + storageState: setup完了後に状態を読み込み
  {
    name: 'chromium',
    use: {
      ...devices['Desktop Chrome'],
      storageState: 'playwright/.auth/user.json',
    },
    dependencies: ['setup'],
  },
]
```

```typescript
// auth.setup.ts — setup project のテストファイル
import { test as setup, expect } from '@playwright/test';

const authFile = 'playwright/.auth/user.json';

setup('authenticate', async ({ page }) => {
  await page.goto('/login');
  await page.getByLabel('Email').fill('user@example.com');
  await page.getByLabel('Password').fill('password');
  await page.getByRole('button', { name: 'Sign in' }).click();
  await page.waitForURL('/dashboard');
  // 3. storageState を保存（Cookie + localStorage）
  await page.context().storageState({ path: authFile });
});
```

`.gitignore` に `playwright/.auth/` を追加すること。

## タイムアウト階層

各レベルの関係と上書き優先度。設定漏れで意図しないタイムアウトが起きやすい:

```typescript
export default defineConfig({
  timeout: 30_000,            // テスト全体（デフォルト 30s）
  globalTimeout: 600_000,     // 全テストスイート（デフォルト 無制限）
  expect: {
    timeout: 5_000,           // expect() アサーション（デフォルト 5s）
  },
  use: {
    actionTimeout: 10_000,    // click, fill 等の個別アクション（デフォルト 無制限）
    navigationTimeout: 30_000,// goto, waitForURL 等（デフォルト 無制限）
  },
});
```

**落とし穴**: `actionTimeout` のデフォルトは**無制限**。テスト全体の `timeout` に達するまでハングする。CI では明示的に設定すべき。

## 複数webServer

フロントエンド + API サーバーの同時起動:

```typescript
webServer: [
  {
    command: 'npm run dev:frontend',
    url: 'http://localhost:3000',
    reuseExistingServer: !process.env.CI,
  },
  {
    command: 'npm run dev:api',
    url: 'http://localhost:8080',
    reuseExistingServer: !process.env.CI,
  },
]
```

**注意**: 配列の順序で起動される。依存関係がある場合はAPI → フロントエンドの順にすること。
