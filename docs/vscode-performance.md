# VSCode パフォーマンス最適化ガイド

## 概要

VSCode の起動速度・応答性・メモリ使用量を最適化するためのベストプラクティス集。
2025-2026年時点の最新情報に基づく。

---

## 1. 診断ツール

最適化の前に、まず現状を計測する。

### コマンドパレット（F1）

| コマンド | 用途 |
|---------|------|
| `Developer: Startup Performance` | 起動フェーズごとのミリ秒単位の内訳 |
| `Developer: Show Running Extensions` | 拡張のアクティベーション時間（ソート済み） |
| `Developer: Open Process Explorer` | プロセスごとのCPU/メモリ使用量 |
| `Help: Start Extension Bisect` | 二分探索で問題拡張を特定（O(log N)） |
| `Toggle Developer Tools` | Chrome DevToolsでレンダラープロセスをプロファイル |

### CLI オプション

```bash
code --disable-extensions   # 拡張なしで起動（ベースライン測定）
code --prof-startup         # 起動時のCPUプロファイルを生成
code --status               # プロセス・ワークスペース情報を表示
code --verbose              # 詳細ログ出力
code --disable-gpu          # GPU アクセラレーション無効化
```

---

## 2. 起動パフォーマンス

### 設定

```jsonc
{
  "workbench.startupEditor": "newUntitledFile",  // "none"で最速だがUX低下
  "workbench.editor.restoreViewState": false,     // ビューステート復元を無効化
  "files.restoreUndoStack": false,                // Undoスタック復元を無効化
  "window.restoreWindows": "one"                  // 前回のウィンドウ復元を制限
}
```

### 拡張管理

**最も効果が高い最適化**。拡張 40→20 で起動時間 26-37% 改善の実測値あり。

- `Developer: Show Running Extensions` でアクティベーション時間を確認
- **1000ms 超**の拡張は無効化・代替を検討
- `*` アクティベーションイベントの拡張を避ける（起動をブロック）
- VSCode Profiles で用途別に拡張セットを切り替え

| 拡張例 | アクティベーション時間 | 評価 |
|--------|---------------------|------|
| ESLint | 39ms | 優秀 |
| GitLens | 35ms | 優秀 |
| Prettier | 286ms | 許容範囲 |
| Live Server | 2513ms | 要改善 |

### プロファイル活用

```bash
code ~/project --profile "Minimal"  # 最小構成プロファイルで起動
```

- 言語・用途ごとにプロファイルを作成（React用、Python用、執筆用 等）
- ワークスペースにプロファイルを関連付けて自動切り替え
- `File > Preferences > Profiles` で管理

---

## 3. エディタ・UI パフォーマンス

### 高効果設定

```jsonc
{
  "editor.minimap.enabled": false,              // ミニマップ無効化
  "editor.codeLens": false,                     // CodeLens 無効化
  "editor.accessibilitySupport": "off",         // アクセシビリティ無効化（大きなファイルで顕著）
  "editor.largeFileOptimizations": true,         // 大ファイルの自動最適化
  "editor.bracketPairColorization.enabled": true, // ネイティブ実装は10,000倍高速
  "editor.renderWhitespace": "selection",        // 全体表示を避ける
  "editor.cursorBlinking": "solid",              // アニメーション削減
  "breadcrumbs.enabled": false                   // パンくずリスト無効化
}
```

### フォーマット関連

```jsonc
{
  "editor.formatOnSave": false,   // 保存時フォーマット無効化
  "editor.formatOnType": false,   // 入力時フォーマット無効化
  "editor.formatOnPaste": false   // ペースト時フォーマット無効化
}
```

> **注意**: CI/pre-commit hook でフォーマットを担保している場合のみ推奨。

### IntelliSense

```jsonc
{
  "editor.suggest.localityBonus": true,   // ローカル変数を優先
  "editor.quickSuggestions": {
    "other": true,
    "comments": false,
    "strings": false
  },
  "editor.quickSuggestionsDelay": 10,
  "editor.semanticHighlighting.enabled": "configuredByTheme"
}
```

---

## 4. ファイルウォッチャー最適化

**npm install が2倍遅くなる**など、最もインパクトの大きい設定の一つ。

```jsonc
{
  "files.watcherExclude": {
    "**/.git/objects/**": true,
    "**/.git/subtree-cache/**": true,
    "**/node_modules/**": true,
    "**/dist/**": true,
    "**/build/**": true,
    "**/.next/**": true,
    "**/coverage/**": true,
    "**/__generated__/**": true,
    "**/.venv/**": true,
    "**/venv/**": true,
    "**/tmp/**": true
  }
}
```

---

## 5. 検索最適化

### 3層除外戦略

| 設定 | 効果 |
|------|------|
| `files.exclude` | エクスプローラーから非表示 |
| `search.exclude` | 全文検索から除外 |
| `files.watcherExclude` | ファイル監視から除外 |

`files.exclude` のパターンは `search.exclude` にも自動適用される。

```jsonc
{
  "files.exclude": {
    "**/.git": true,
    "**/.DS_Store": true,
    "**/node_modules": true
  },
  "search.exclude": {
    "**/node_modules": true,
    "**/dist": true,
    "**/build": true,
    "**/.next": true,
    "**/coverage": true,
    "**/__generated__": true
  },
  "search.followSymlinks": false  // シンボリックリンク追跡を無効化
}
```

> 大規模プロジェクトで検索速度 50-80% 改善の報告あり。

---

## 6. Git パフォーマンス

```jsonc
{
  "git.autorefresh": false,     // 5秒ごとのポーリングを無効化
  "git.autofetch": false,       // 自動フェッチ無効化
  "git.decorations.enabled": true, // ファイルデコレーション（軽量）
  "scm.autoReveal": false       // SCMパネルの自動表示無効化
}
```

大規模リポジトリでは git config 側の最適化も有効:

```bash
git config core.fsmonitor true
git config core.untrackedcache true
```

---

## 7. TypeScript パフォーマンス

```jsonc
{
  "typescript.tsserver.log": "off",
  "typescript.disableAutomaticTypeAcquisition": true,
  "typescript.tsserver.maxTsServerMemory": 3072
}
```

tsconfig.json 側:

```jsonc
{
  "compilerOptions": {
    "skipLibCheck": true  // 型定義ファイルのチェックをスキップ
  }
}
```

キャッシュクリア（問題発生時）:
- macOS: `~/Library/Caches/Microsoft/TypeScript`

---

## 8. メモリ管理

```jsonc
{
  "workbench.editor.limit.enabled": true,        // エディタ上限を有効化
  "workbench.editor.limit.value": 10,             // 最大10タブ
  "workbench.editor.limit.perEditorGroup": false,
  "workbench.editor.closeOnFileDelete": true,
  "files.maxMemoryForLargeFilesMB": 4096
}
```

### メモリ削減のベストプラクティス

1. 拡張を最小限に保つ
2. `files.watcherExclude` を適切に設定
3. エディタタブ数を制限
4. 定期的に VSCode を再起動（メモリリーク対策）
5. Process Explorer で異常なプロセスを監視

---

## 9. バックグラウンドタスク抑制

```jsonc
{
  "telemetry.telemetryLevel": "off",           // テレメトリ無効化
  "update.mode": "manual",                     // 自動更新チェック無効化
  "extensions.autoUpdate": false,              // 拡張の自動更新無効化
  "update.showReleaseNotes": false,            // リリースノート非表示
  "extensions.ignoreRecommendations": true     // 拡張推奨を無効化
}
```

---

## 10. トラブルシューティングフロー

パフォーマンス問題が発生した場合:

1. `code --disable-extensions` で起動 → 拡張が原因か切り分け
2. `Developer: Startup Performance` で起動内訳を確認
3. `Developer: Show Running Extensions` で遅い拡張を特定
4. `Help: Start Extension Bisect` で問題拡張を二分探索
5. `Developer: Open Process Explorer` でCPU/メモリ異常を確認
6. `Toggle Developer Tools` → Performance タブで30-60秒プロファイル
7. 設定をリセットして切り分け（`settings.json` → `{}`）

---

## 参考資料

### 公式ドキュメント

- [Performance Issues Wiki](https://github.com/microsoft/vscode/wiki/Performance-Issues) - 公式トラブルシューティング
- [File Watcher Issues Wiki](https://github.com/microsoft/vscode/wiki/File-Watcher-Issues) - ファイルウォッチャー問題
- [Extension Bisect](https://code.visualstudio.com/blogs/2021/02/16/extension-bisect) - 二分探索機能の解説
- [VSCode Profiles](https://code.visualstudio.com/docs/configure/profiles) - プロファイル機能
- [Bracket Pair Colorization](https://code.visualstudio.com/blogs/2021/09/29/bracket-pair-colorization) - ネイティブ実装の技術解説

### コミュニティ

- [VSCode Faster Guide](https://vscode.one/make-vscode-faster/) - 実測値付きの最適化ガイド
- [FreeCodeCamp: Optimize VSCode](https://www.freecodecamp.org/news/optimize-vscode-performance-best-extensions/) - 包括的な最適化手順
- [Apidog: 10x Faster Settings](https://apidog.com/blog/vscode-settings-to-make-vscode-10x-faster/) - 設定一覧
