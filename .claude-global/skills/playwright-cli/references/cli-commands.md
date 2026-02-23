# Interactive CLI Commands

インタラクティブモードの全コマンドリファレンス。
元 `microsoft/playwright-cli` の機能。現在は `npx playwright` に統合。

## TOC

- [Navigation](#navigation)
- [Snapshot](#snapshot)
- [Interaction](#interaction)
- [Keyboard / Mouse](#keyboard--mouse)
- [Tabs](#tabs)
- [Storage & State](#storage--state)
- [Media](#media)
- [Session Management](#session-management)
- [Advanced](#advanced)

## Navigation

| コマンド | 説明 |
|----------|------|
| `open [url]` | ブラウザ起動。URL指定で遷移 |
| `goto url` | 現在のタブでURL遷移 |
| `go-back` | 戻る |
| `go-forward` | 進む |
| `reload` | ページ再読込 |
| `close` | ブラウザ終了 |

## Snapshot

```bash
npx playwright snapshot              # アクセシビリティツリー + 要素ref取得
npx playwright snapshot page.txt     # ファイルに保存
```

操作後はページ状態が変わるため、再度 `snapshot` で ref を取得する。

## Interaction

| コマンド | 説明 | 例 |
|----------|------|----|
| `click ref` | クリック | `click e4` |
| `dblclick ref` | ダブルクリック | `dblclick e1` |
| `fill ref value` | 入力欄の値を置換 | `fill e3 "user@test.com"` |
| `type ref text` | キーストロークで入力 | `type e3 "hello"` |
| `hover ref` | ホバー | `hover e2` |
| `select ref values` | ドロップダウン選択 | `select e5 "option1"` |
| `check ref` | チェックボックスON | `check e6` |
| `uncheck ref` | チェックボックスOFF | `uncheck e6` |
| `drag startRef endRef` | ドラッグ&ドロップ | `drag e1 e2` |

`fill` vs `type`: `fill` は既存値をクリアして置換。`type` はキーストローク単位の入力（IME・オートコンプリート対応）。

## Keyboard / Mouse

| コマンド | 説明 | 例 |
|----------|------|----|
| `press key` | キー送信 | `press Enter`, `press Control+a` |
| `keydown key` | キー押下 | `keydown Shift` |
| `keyup key` | キー解放 | `keyup Shift` |
| `mousemove x y` | マウス移動 | `mousemove 100 200` |
| `mousedown [button]` | マウスボタン押下 | `mousedown left` |
| `mouseup [button]` | マウスボタン解放 | `mouseup left` |
| `mousewheel deltaX deltaY` | スクロール | `mousewheel 0 300` |

## Tabs

| コマンド | 説明 |
|----------|------|
| `tab-list` | 開いているタブ一覧 |
| `tab-new [url]` | 新規タブ |
| `tab-select index` | タブ切替（0始まり） |
| `tab-close [index]` | タブを閉じる |

## Storage & State

`cookie-`, `localstorage-`, `sessionstorage-` の3プレフィックスで共通操作:
`list`, `get name`, `set name value`, `delete name`, `clear`

### 状態の保存・復元

```bash
npx playwright state-save auth.json      # Cookie + localStorage を保存
npx playwright state-load auth.json      # 保存した状態を復元
```

認証フローの結果を保存して再利用する場合に有効。

## Media

| コマンド | 説明 | 例 |
|----------|------|----|
| `screenshot [ref] [file]` | スクリーンショット | `screenshot page.png` |
| `pdf [file]` | PDF出力 | `pdf report.pdf` |
| `video-start` | 録画開始 | `video-start` |
| `video-stop file` | 録画停止・保存 | `video-stop demo.webm` |
| `tracing-start` | トレース記録開始 | `tracing-start` |
| `tracing-stop` | トレース記録停止 | `tracing-stop` |

## Session Management

```bash
npx playwright -s session1 open https://a.com   # 名前付きセッション作成
npx playwright -s session2 open https://b.com   # 別セッション（並行操作可）
npx playwright list                               # アクティブセッション一覧
npx playwright close-all                          # 全セッション終了
npx playwright kill-all                           # 全プロセス強制終了
npx playwright delete-data                        # 永続化データ削除
```

| オプション | 説明 |
|-----------|------|
| `-s name` | セッション名指定（デフォルト: "default"） |
| `--persistent` | プロファイルデータを永続化 |
| `--profile=path` | カスタムプロファイルディレクトリ |

## Advanced

| コマンド | 説明 | 例 |
|----------|------|----|
| `run-code js` | JavaScript実行 | `run-code "document.title"` |
| `route pattern [status] [body]` | ネットワークモック | `route "**/api/*" 200 '{"ok":true}'` |
| `route-list` | 設定済みルート一覧 | |
| `unroute pattern` | ルート解除 | |
| `network` | ネットワークリクエスト一覧 | |
| `console [type]` | コンソールメッセージ一覧 | `console error` |
| `dialog-accept [text]` | ダイアログ承認 | `dialog-accept "OK"` |
| `dialog-dismiss` | ダイアログ却下 | |
