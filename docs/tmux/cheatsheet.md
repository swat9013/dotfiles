# Tmux & Tmuxinator チートシート

> Prefix: `C-z` (Ctrl+z)

## セッション操作

| コマンド | 機能 |
|----------|------|
| `tmux new -s <name>` | 名前付きセッション作成 |
| `tmux ls` | セッション一覧 |
| `tmux attach -t <name>` | アタッチ |
| `tmux attach -d -t <name>` | 他を切断してアタッチ |
| `tmux kill-session -t <name>` | セッション終了 |
| `prefix + d` | デタッチ |
| `prefix + s` | セッション選択 |
| `prefix + $` | セッション名変更 |

## ウィンドウ操作

| キー | 機能 |
|------|------|
| `prefix + c` | 新規ウィンドウ（現在のパス） |
| `prefix + ,` | ウィンドウ名変更 |
| `prefix + w` | ウィンドウ一覧 |
| `prefix + &` | ウィンドウ削除 |
| `prefix + <数字>` | ウィンドウ切替 |
| `Shift + →/←` | 次/前のウィンドウ（prefix不要） |

## ペイン操作

| キー | 機能 |
|------|------|
| `prefix + \|` | 垂直分割（左右） |
| `prefix + -` | 水平分割（上下） |
| `prefix + x` | ペイン削除 |
| `prefix + z` | ペイン最大化/復帰 |
| `prefix + q` | ペイン番号表示 |
| `prefix + {` / `}` | ペイン入替 |
| `prefix + Space` | レイアウト切替 |
| `Ctrl + →/←/↑/↓` | ペイン移動（prefix不要） |
| `prefix + e` | ペイン同期トグル |

## コピーモード（vi）

| キー | 機能 |
|------|------|
| `prefix + [` | コピーモード開始 |
| `v` | 選択開始 |
| `y` | コピー（pbcopy） |
| `q` | コピーモード終了 |
| `h/j/k/l` | カーソル移動 |
| `/` | 前方検索 |
| `?` | 後方検索 |
| `n` / `N` | 次/前の検索結果 |
| マウスドラッグ | 選択→自動コピー |

## プラグイン操作（TPM）

| キー | 機能 |
|------|------|
| `prefix + I` | プラグインインストール |
| `prefix + U` | プラグイン更新 |
| `prefix + Ctrl-s` | セッション保存（resurrect） |
| `prefix + Ctrl-r` | セッション復元（resurrect） |

## 設定管理

| キー/コマンド | 機能 |
|---------------|------|
| `prefix + r` | 設定リロード |
| `tmux list-keys` | キーバインド一覧 |
| `tmux show-options -g` | グローバルオプション一覧 |
| `tmux list-windows` | カスタムレイアウト文字列取得 |

---

## Tmuxinator

### 基本コマンド

| コマンド | 機能 |
|----------|------|
| `tmuxinator new <name>` | 新規プロジェクト作成 |
| `tmuxinator start <name>` | セッション開始 |
| `tmuxinator stop <name>` | セッション停止 |
| `tmuxinator list` | プロジェクト一覧 |
| `tmuxinator edit <name>` | 設定編集 |
| `tmuxinator copy <src> <dst>` | プロジェクト複製 |
| `tmuxinator debug <name>` | 実行コマンド確認 |
| `tmuxinator doctor` | 設定診断 |

### 設定ファイルの場所

| パス | 用途 |
|------|------|
| `~/.tmuxinator/<name>.yml` | グローバル設定 |
| `./.tmuxinator.yml` | ローカルプロジェクト設定 |

### YAML設定の基本構造

```yaml
name: project-name
root: ~/projects/myproject
startup_window: editor

windows:
  - editor:                    # ウィンドウ名: コマンド
      layout: main-vertical    # レイアウト指定
      panes:
        - vim .                # ペイン1
        - console              # ペイン2
  - server: rails s            # シンプル形式
  - shell:                     # 空のシェル
```

### 定義済みレイアウト

| レイアウト | 説明 |
|------------|------|
| `even-horizontal` | 水平均等分割 |
| `even-vertical` | 垂直均等分割 |
| `main-horizontal` | メイン上 + 残り下 |
| `main-vertical` | メイン左 + 残り右 |
| `tiled` | タイル配置 |

### フック

| フック | タイミング |
|--------|------------|
| `on_project_start` | セッション作成前 |
| `on_project_stop` | `stop`コマンド時 |
| `pre_window` | 各ウィンドウ・ペインの前 |

### ERBテンプレート

```yaml
# 環境変数
name: <%= ENV["PROJECT_NAME"] || "default" %>

# 引数: tmuxinator start proj arg1 key=value
root: ~/projects/<%= @args[0] %>
```
