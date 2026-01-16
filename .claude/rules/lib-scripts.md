---
paths: lib/**
---

# lib/ ユーティリティスクリプト

## シンボリックリンク管理 (dotfilesLink.sh)

`lib/dotfilesLink.sh` が設定ファイルの同期を管理。

### 基本動作

```
~/.dotfiles/.zshrc      →  ~/.zshrc
~/.dotfiles/.gitconfig  →  ~/.gitconfig
~/.dotfiles/sheldon/    →  ~/.config/sheldon/
~/.dotfiles/ghostty/    →  ~/.config/ghostty/
```

### 除外対象

- `.git`, `.gitignore`, `.gitmodule`
- `.gitconfig.local.sample` (`.gitconfig.local` にコピーして使用)
- `.claude`, `.claude-global`

### 特別処理

以下のディレクトリ・ファイルは特別なパスにリンク:

| 元 | リンク先 |
|----|---------|
| `sheldon/` | `~/.config/sheldon` |
| `ghostty/` | `~/.config/ghostty` |
| `.claude-global/settings.json` | `~/.claude/settings.json` |
| `.claude-global/CLAUDE.md` | `~/.claude/CLAUDE.md` |
| `.claude-global/skills/` | `~/.claude/skills/` |

## スクリプト一覧

| スクリプト | 機能 | 引数 |
|-----------|------|------|
| dotfilesLink.sh | シンボリックリンク作成・管理 | なし |
| auto_update.sh | 24時間経過で自動git pull | なし |
| macos.sh | macOS input source切り替え無効化 | なし |
| ide.sh | tmux IDE用パネル配置 | なし |
| vibe-kanban.sh | npx vibe-kanban の起動・停止管理 | start/stop |
| fortivpn.sh | OpenFortiVPN ラッパー | なし |
| ssh_host_color.sh | SSH ホスト色分け | ホスト名 |

## dotfilesLink.sh の詳細動作

1. `~/.dotfiles` 内の `.` で始まるファイルを検出
2. 除外リストをチェック
3. 既存のシンボリックリンク・ファイルを確認
4. リンク作成 or 警告表示
5. 特別処理対象（sheldon/, ghostty/など）を処理

### 実行タイミング

- 初回インストール時: `install.sh` から呼び出し
- 手動実行: `./lib/dotfilesLink.sh`

## auto_update.sh の動作

`.zshrc` から自動で呼び出され、以下を実行:

1. `~/.last_update` のタイムスタンプをチェック
2. 24時間経過していない場合は終了
3. `git fetch origin master` で最新情報を取得
4. HEAD vs origin/master を比較
5. 差分がある場合は `git pull` 実行
6. `~/.last_update` を更新

### 無効化方法

`.zshrc` から `lib/auto_update.sh` の呼び出しをコメントアウト。

## その他のスクリプト

### ide.sh

tmuxでIDE風のパネルレイアウトを作成:
- 左側: エディタエリア
- 右上: サーバーログ
- 右下: コマンド実行

### vibe-kanban.sh

`npx vibe-kanban` の起動・停止を管理。ポート番号の競合を回避。

### fortivpn.sh

OpenFortiVPN の接続ラッパー。設定ファイルのパスを自動設定。
