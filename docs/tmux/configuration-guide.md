# Tmux設定ガイド

## 1. 概要

本ドキュメントでは、tmuxとtmuxinatorの設定方法、カスタマイズオプション、およびベストプラクティスについてまとめる。

**対象バージョン**: tmux 3.6+ (2025年12月時点)

## 2. 設定ファイル

### 2.1 配置場所

| パス | 優先度 | 備考 |
|------|--------|------|
| `~/.tmux.conf` | 1 | 従来のデフォルト |
| `$XDG_CONFIG_HOME/tmux/tmux.conf` | 2 | XDG準拠（推奨） |
| `~/.config/tmux/tmux.conf` | 2 | XDG_CONFIG_HOME未設定時 |

**注意**: 設定ファイルはサーバー起動時のみ読み込まれる。新規セッション作成時は再読み込みされない。

### 2.2 設定の再読み込み

```bash
# コマンドラインから
tmux source-file ~/.tmux.conf

# キーバインドで（推奨設定）
bind r source-file ~/.tmux.conf \; display "Reloaded!"
```

## 3. オプション体系

### 3.1 オプションの種類

| 種類 | コマンド | スコープ |
|------|----------|----------|
| サーバーオプション | `set -s` | tmuxサーバー全体 |
| セッションオプション | `set -g` | 全セッション（グローバル） |
| ウィンドウオプション | `setw -g` | 全ウィンドウ（グローバル） |

### 3.2 主要オプション一覧

#### サーバーオプション

| オプション | デフォルト | 説明 |
|------------|-----------|------|
| `escape-time` | 500ms | Escキー後の待機時間（Vim使用時は0推奨） |
| `default-terminal` | - | $TERM変数のデフォルト値 |
| `buffer-limit` | 50 | 自動バッファの最大数 |

#### セッションオプション

| オプション | デフォルト | 説明 |
|------------|-----------|------|
| `base-index` | 0 | ウィンドウ番号の開始値（1推奨） |
| `history-limit` | 2000 | スクロールバック行数 |
| `mouse` | off | マウスサポート |
| `status` | on | ステータスバー表示 |
| `status-position` | bottom | ステータスバー位置（top/bottom） |

#### ウィンドウオプション

| オプション | デフォルト | 説明 |
|------------|-----------|------|
| `mode-keys` | emacs | コピーモードのキーバインド（vi推奨） |
| `pane-base-index` | 0 | ペイン番号の開始値 |
| `automatic-rename` | on | ウィンドウ名の自動更新 |

## 4. キーバインド

### 4.1 構文

```bash
# 基本形式
bind-key [-n] [-T table] key command

# エイリアス
bind key command      # prefix + key
bind -n key command   # prefix不要（rootテーブル）
```

**修飾キー**:
- `C-`: Control
- `S-`: Shift
- `M-`: Alt/Meta

### 4.2 キーテーブル

| テーブル | 説明 | 使用例 |
|----------|------|--------|
| `prefix` | プレフィックス後のキー | `bind c new-window` |
| `root` | プレフィックス不要 | `bind -n M-Left select-pane -L` |
| `copy-mode-vi` | viコピーモード | `bind -T copy-mode-vi v send -X begin-selection` |

### 4.3 推奨キーバインド

```bash
# プレフィックスキー変更（C-aまたはC-z）
set -g prefix C-a
unbind C-b
bind C-a send-prefix

# 直感的なペイン分割
bind | split-window -h -c "#{pane_current_path}"
bind - split-window -v -c "#{pane_current_path}"

# プレフィックス不要のペイン移動
bind -n M-Left select-pane -L
bind -n M-Right select-pane -R
bind -n M-Up select-pane -U
bind -n M-Down select-pane -D

# プレフィックス不要のウィンドウ移動
bind -n S-Left previous-window
bind -n S-Right next-window
```

## 5. コピーモード

### 5.1 viモード設定

```bash
# viキーバインドを有効化
setw -g mode-keys vi

# 選択とコピーをVim風に
bind -T copy-mode-vi v send -X begin-selection
bind -T copy-mode-vi y send -X copy-selection-and-cancel

# マウスドラッグでコピー（macOS）
bind -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel "pbcopy"
```

### 5.2 主要キーバインド（viモード）

| キー | 機能 |
|------|------|
| `[` | コピーモード開始（prefix後） |
| `Space` | 選択開始 |
| `Enter` | コピー実行 |
| `/` | 前方検索 |
| `?` | 後方検索 |
| `n` / `N` | 次/前の検索結果 |

## 6. ステータスバー

### 6.1 構造

```
[左側] ─────────── [中央：ウィンドウリスト] ─────────── [右側]
```

### 6.2 設定オプション

```bash
# 位置と更新間隔
set -g status-position top
set -g status-interval 1

# スタイル
set -g status-style "fg=white,bg=black"

# 左右のコンテンツ
set -g status-left "#[fg=green]#H"
set -g status-right "#[fg=yellow]%Y-%m-%d %H:%M"
set -g status-left-length 40
set -g status-right-length 60
```

### 6.3 フォーマット変数

| 変数 | 説明 |
|------|------|
| `#H` | ホスト名 |
| `#S` | セッション名 |
| `#I` | ウィンドウインデックス |
| `#P` | ペインインデックス |
| `#W` | ウィンドウ名 |
| `#T` | ペインタイトル |

### 6.4 色指定

```bash
# 基本色（名前指定）
#[fg=red,bg=blue]

# 256色
#[fg=colour231,bg=colour234]

# シェルコマンド実行
#(uptime | cut -d',' -f1)
```

## 7. プラグイン管理（TPM）

### 7.1 インストール

```bash
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

### 7.2 設定

```bash
# プラグイン宣言
set -g @plugin 'tmux-plugins/tpm'
set -g @plugin 'tmux-plugins/tmux-sensible'
set -g @plugin 'tmux-plugins/tmux-resurrect'
set -g @plugin 'tmux-plugins/tmux-continuum'

# TPM初期化（.tmux.confの最後に記述）
run '~/.tmux/plugins/tpm/tpm'
```

### 7.3 キーバインド

| キー | 機能 |
|------|------|
| `prefix + I` | プラグインインストール |
| `prefix + U` | プラグイン更新 |
| `prefix + alt + u` | プラグイン削除 |

### 7.4 推奨プラグイン

| プラグイン | 説明 |
|------------|------|
| tmux-sensible | 合理的なデフォルト設定 |
| tmux-resurrect | セッションの保存/復元（手動） |
| tmux-continuum | セッションの自動保存/復元 |
| tmux-yank | クリップボード連携強化 |

#### tmux-resurrect / continuum 設定

```bash
# 自動復元を有効化
set -g @continuum-restore 'on'

# 保存間隔（分）
set -g @continuum-save-interval '15'

# 手動操作
# prefix + Ctrl-s: 保存
# prefix + Ctrl-r: 復元
```

## 8. セッション管理

### 8.1 基本コマンド

```bash
# セッション操作
tmux new -s <name>           # 新規セッション（名前付き）
tmux ls                      # セッション一覧
tmux attach -t <name>        # アタッチ
tmux attach -d -t <name>     # 他クライアントをデタッチしてアタッチ
tmux kill-session -t <name>  # セッション終了
```

### 8.2 キーバインド（prefix後）

| キー | 機能 |
|------|------|
| `d` | デタッチ |
| `s` | セッション選択 |
| `$` | セッション名変更 |
| `(` / `)` | 前/次のセッション |

## 9. ベストプラクティス

### 9.1 設定構成

```bash
# 1. 基本設定
set -sg escape-time 0
set -g history-limit 50000
set -g base-index 1
setw -g pane-base-index 1

# 2. キーバインド
# ... (上記参照)

# 3. 外観設定
set -g default-terminal "screen-256color"
set -g status-position top

# 4. プラグイン
# ... (上記参照)
```

### 9.2 セッション設計

| 単位 | 用途 | 例 |
|------|------|-----|
| セッション | プロジェクト | `client-website`, `server-api` |
| ウィンドウ | タスク | `editor`, `server`, `logs` |
| ペイン | サブタスク | エディタ横のターミナル |

### 9.3 避けるべきこと

- 無関係なタスクを1つのセッションに混在させる
- プレフィックスを頻繁に使うキー（`C-a`のbash行頭移動）に設定
- 過度に複雑なステータスバー（更新間隔を考慮）

## 10. 参考資料

### 公式ドキュメント

- [tmux Wiki - Getting Started](https://github.com/tmux/tmux/wiki/Getting-Started)
- [tmux Wiki - Modifier Keys](https://github.com/tmux/tmux/wiki/Modifier-Keys)

### プラグイン

- [TPM (Tmux Plugin Manager)](https://github.com/tmux-plugins/tpm)
- [tmux-plugins/list](https://github.com/tmux-plugins/list) - プラグイン一覧
- [tmux-resurrect](https://github.com/tmux-plugins/tmux-resurrect)
- [tmux-continuum](https://github.com/tmux-plugins/tmux-continuum)

### ガイド

- [Make tmux Pretty and Usable](https://hamvocke.com/blog/a-guide-to-customizing-your-tmux-conf/) - カスタマイズガイド
- [How to configure tmux, from scratch](https://ianthehenry.com/posts/how-to-configure-tmux/) - 詳細設定解説
- [Binding Keys in tmux](https://www.seanh.cc/2020/12/28/binding-keys-in-tmux/) - キーバインド詳解
- [tmux Status Bar Customization](https://www.baeldung.com/linux/tmux-status-bar-customization) - ステータスバー設定
