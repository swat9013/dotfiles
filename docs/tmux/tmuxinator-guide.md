# Tmuxinator設定ガイド

## 1. 概要

Tmuxinatorは、tmuxセッションをYAML設定ファイルで定義・管理するRuby製ツール。複雑なセッション構成を再現可能な形で保存し、ワンコマンドで起動できる。

## 2. インストール

```bash
# RubyGems（推奨）
gem install tmuxinator

# Homebrew（一部制限あり）
brew install tmuxinator
```

**依存**: Ruby, tmux

## 3. 設定ファイル

### 3.1 配置場所

| パス | 用途 |
|------|------|
| `~/.tmuxinator/` | プロジェクト設定ファイル |
| `~/.config/tmuxinator/` | XDG準拠の配置場所 |
| `./.tmuxinator.yml` | ローカルプロジェクト設定 |

### 3.2 基本コマンド

```bash
tmuxinator new <project>     # 新規プロジェクト作成
tmuxinator start <project>   # セッション開始
tmuxinator stop <project>    # セッション停止
tmuxinator list              # プロジェクト一覧
tmuxinator copy <src> <dst>  # プロジェクト複製
tmuxinator debug <project>   # 実行コマンド確認
tmuxinator doctor            # 設定診断
```

## 4. YAML設定構造

### 4.1 基本テンプレート

```yaml
name: myproject
root: ~/projects/myproject

# オプション
socket_name: foo                    # カスタムソケット名
attach: true                        # 自動アタッチ（デフォルト: true）
startup_window: editor              # 起動時のアクティブウィンドウ
startup_pane: 0                     # 起動時のアクティブペイン
tmux_options: -f ~/.tmux.mac.conf   # tmuxへの追加オプション
tmux_command: tmux                  # tmuxコマンドパス
enable_pane_titles: true            # ペインタイトル有効化（tmux 2.6+）

# ウィンドウ定義
windows:
  - editor: vim
  - server: bundle exec rails s
  - logs: tail -f log/development.log
```

### 4.2 ウィンドウ設定

#### シンプル形式

```yaml
windows:
  - editor: vim
  - server: bundle exec rails s
```

#### 詳細形式

```yaml
windows:
  - editor:
      layout: main-vertical
      panes:
        - vim
        - guard
  - database:
      root: ~/projects/db
      panes:
        - mycli
```

#### 名前なしウィンドウ

```yaml
windows:
  - ~:          # null値でデフォルト名
      panes:
        - htop
```

### 4.3 ペイン設定

```yaml
windows:
  - development:
      layout: main-vertical
      panes:
        - vim                           # 単一コマンド
        - guard                         # 単一コマンド
        -                               # 空のペイン
        - pane_with_commands:           # 複数コマンド
            - cd ~/projects
            - git status
```

### 4.4 レイアウト

#### 定義済みレイアウト

| レイアウト | 説明 |
|------------|------|
| `even-horizontal` | ペインを水平に均等分割 |
| `even-vertical` | ペインを垂直に均等分割 |
| `main-horizontal` | 1つのメインペイン（上）+ 残りを下に配置 |
| `main-vertical` | 1つのメインペイン（左）+ 残りを右に配置 |
| `tiled` | タイル状に配置 |

#### カスタムレイアウト

```yaml
windows:
  - editor:
      layout: "a]0,318x99,0,0{212x99,0,0,1,105x99,213,0[105x49,213,0,2,105x49,213,50,3]}"
```

**取得方法**: 手動でレイアウトを作成後、`tmux list-windows` でレイアウト文字列を取得。

## 5. フック

### 5.1 利用可能なフック

| フック | 実行タイミング |
|--------|----------------|
| `on_project_start` | セッション作成前（毎回） |
| `on_project_first_start` | 初回起動時のみ |
| `on_project_restart` | 再起動時 |
| `on_project_exit` | デタッチ時 |
| `on_project_stop` | `tmuxinator stop` 時 |
| `pre` | ウィンドウ単位でペイン作成前 |
| `pre_window` | 全ウィンドウ・ペインでコマンド実行前 |

### 5.2 使用例

```yaml
name: myproject
root: ~/projects/myproject

# プロジェクト開始前に依存関係を更新
on_project_start:
  - git fetch origin
  - bundle install --quiet

# デタッチ時にクリーンアップ
on_project_exit:
  - echo "Goodbye!" >> /tmp/tmuxinator.log

# 各ウィンドウ・ペインでバージョンマネージャ初期化
pre_window: eval "$(rbenv init -)"

windows:
  - editor:
      pre:
        - nvm use 18     # このウィンドウのみ
      panes:
        - vim
```

## 6. テンプレート機能（ERB）

### 6.1 環境変数

```yaml
name: <%= ENV["PROJECT_NAME"] || "default" %>
root: <%= ENV["PROJECT_ROOT"] %>

windows:
  - server: rails s -p <%= ENV["PORT"] || 3000 %>
```

### 6.2 引数

```bash
tmuxinator start myproject arg1 arg2 key=value
```

```yaml
name: myproject
root: ~/projects/<%= @args[0] %>

windows:
  - editor: vim <%= @args[1] %>
  - server: rails s -p <%= @settings["port"] || 3000 %>
```

### 6.3 条件分岐

```yaml
windows:
  <% if ENV["RAILS_ENV"] == "production" %>
  - monitor:
      panes:
        - htop
        - tail -f /var/log/syslog
  <% end %>
  - editor: vim
```

## 7. 実践的な設定例

### 7.1 Rails開発環境

```yaml
name: rails-app
root: ~/projects/myapp

on_project_start:
  - bundle install --quiet
  - bin/rails db:migrate

pre_window: eval "$(rbenv init -)"

windows:
  - editor:
      layout: main-vertical
      panes:
        - vim .
        - bin/rails console
  - server: bin/rails server
  - worker: bundle exec sidekiq
  - logs: tail -f log/development.log
```

### 7.2 フロントエンド開発

```yaml
name: frontend
root: ~/projects/webapp

pre_window: nvm use 20

windows:
  - editor: code .
  - dev:
      layout: even-horizontal
      panes:
        - npm run dev
        - npm run test:watch
  - shell:
```

### 7.3 マイクロサービス開発

```yaml
name: microservices
root: ~/projects

windows:
  - api:
      root: ~/projects/api-service
      panes:
        - docker-compose up
  - frontend:
      root: ~/projects/frontend
      panes:
        - npm run dev
  - gateway:
      root: ~/projects/gateway
      panes:
        - go run main.go
```

## 8. ベストプラクティス

### 8.1 設定のポイント

| 項目 | 推奨 |
|------|------|
| プロジェクト名 | `.`（ドット）を含めない（tmuxのインデックス区切りと競合） |
| インデント | YAML仕様に厳密に従う（スペース2つ推奨） |
| 複雑なコマンド | 別スクリプトに切り出し、`source`で呼び出し |
| バージョンマネージャ | `pre_window`で初期化 |

### 8.2 YAMLの注意点

```yaml
# NG: インデントが不正確
windows:
- editor: vim    # ハイフン前にスペースが必要

# OK
windows:
  - editor: vim
```

### 8.3 シンプルに保つ

- 基本的なウィンドウ・ペイン構成から始める
- 過度に複雑な自動化は避ける（デバッグが困難）
- 環境固有の設定は環境変数で外出し

### 8.4 デバッグ

```bash
# 実行されるtmuxコマンドを確認
tmuxinator debug myproject

# 設定の検証
tmuxinator doctor
```

## 9. tmux-resurrect との比較

| 観点 | Tmuxinator | tmux-resurrect |
|------|------------|----------------|
| 用途 | 事前定義されたセッション構成 | 既存セッションの保存/復元 |
| 設定 | YAML | 不要（プラグイン） |
| 柔軟性 | 高（ERBテンプレート） | 低（スナップショット） |
| 自動化 | ○ | △（continuum併用で可） |
| 適用場面 | プロジェクト固有の環境 | 作業中断時の復旧 |

**併用推奨**: tmuxinatorで構成を定義し、resurrect/continuumで作業状態を保持。

## 10. 参考資料

### 公式

- [Tmuxinator GitHub](https://github.com/tmuxinator/tmuxinator)
- [Tmuxinator公式サイト](https://tmuxinator.com/)

### ガイド

- [Templating tmux with tmuxinator](https://thoughtbot.com/blog/templating-tmux-with-tmuxinator) - ERBテンプレート活用
- [Managing Development Environments with Tmux and Tmuxinator](https://jessarcher.com/articles/managing-development-environments-with-tmux-and-tmuxinator/) - 実践ガイド
- [Tmux and Tmuxinator Workflow](https://www.simplethread.com/tmux-and-tmuxinator-workflow/) - ワークフロー設計

### サンプル設定

- [sample.yml (公式)](https://github.com/tmuxinator/tmuxinator/blob/master/spec/fixtures/sample.yml)
- [assets/sample.yml (テンプレート)](https://github.com/tmuxinator/tmuxinator/blob/master/lib/tmuxinator/assets/sample.yml)
