---
name: glab
description: GitLab CLI操作ガイド。MR・Issue・CI/CD操作を含む。Use when「glab」「GitLab CLI」「GitLab MR」「GitLab CI」。
user-invocable: false
---

# glab

GitLab をコマンドラインで操作するための CLI ツール。

## 認証

```bash
glab auth login                   # 対話的にログイン
glab auth login --hostname gitlab.example.com  # Self-Managed
glab auth status                  # 認証状態確認
glab auth logout                  # ログアウト
```

## Merge Request

```bash
# 作成
glab mr create                    # 対話的に作成
glab mr create --fill             # コミットから自動入力
glab mr create --fill --label bugfix
glab mr create -a @me --reviewer user1
glab mr create --draft            # Draft MR
glab mr for 42                    # issue #42 に対する MR 作成

# 一覧・表示
glab mr list                      # オープン MR 一覧
glab mr list --state merged       # マージ済み一覧
glab mr list --assignee @me       # 自分担当
glab mr view 123                  # 詳細表示
glab mr view 123 --web            # ブラウザで開く
glab mr diff 123                  # 差分表示

# レビュー・承認
glab mr approve 123               # 承認
glab mr revoke 123                # 承認取消
glab mr approvers 123             # 承認者一覧
glab mr note 123 -m "LGTM"       # コメント追加

# 操作
glab mr checkout 123              # ローカルにチェックアウト
glab mr merge 123                 # マージ
glab mr merge 123 --squash        # スカッシュマージ
glab mr rebase 123                # リベース
glab mr close 123                 # クローズ
glab mr reopen 123                # 再オープン
glab mr update 123 --title "new"  # 更新
glab mr delete 123                # 削除
```

## Issue

```bash
# 作成・一覧
glab issue create                 # 対話的に作成
glab issue create --label bug --assignee @me
glab issue list                   # オープン issue 一覧
glab issue list --label bug       # ラベルでフィルタ
glab issue view 42                # 詳細表示
glab issue view 42 --web          # ブラウザで開く

# 操作
glab issue note 42 -m "対応中"    # コメント追加
glab issue close 42               # クローズ
glab issue reopen 42              # 再オープン
glab issue update 42 --label fix  # 更新
glab issue delete 42              # 削除
```

## CI/CD パイプライン

```bash
# パイプライン
glab ci list                      # パイプライン一覧
glab ci status                    # 現在のブランチのパイプライン状態
glab ci view                      # 対話的にパイプライン閲覧
glab ci get                       # JSON でパイプライン情報取得
glab ci run                       # パイプライン実行
glab ci run -b main               # ブランチ指定で実行
glab ci delete 12345              # パイプライン削除
glab ci cancel                    # 実行中パイプラインをキャンセル

# ジョブ
glab ci trace <job-id>            # ジョブログをリアルタイム追跡
glab ci retry <job-id>            # ジョブ再実行
glab ci trigger <job-id>          # マニュアルジョブをトリガー
glab ci artifact <ref> <job>      # アーティファクトダウンロード

# 設定検証
glab ci lint                      # .gitlab-ci.yml の検証
```

## リポジトリ

```bash
glab repo clone owner/repo        # クローン
glab repo clone -g mygroup        # グループ全リポジトリをクローン
glab repo fork owner/repo         # フォーク
glab repo view                    # 現在のリポジトリ情報
glab repo view --web              # ブラウザで開く
glab repo list                    # リポジトリ一覧
glab repo search "keyword"        # 検索
glab repo create myproject        # 新規作成
```

## リリース

```bash
glab release list                 # リリース一覧
glab release view v1.0.0          # リリース詳細
glab release create v1.0.0        # リリース作成
glab release create v1.0.0 ./dist/*  # ファイル付きリリース
glab release download v1.0.0      # アセットダウンロード
glab release delete v1.0.0        # 削除
```

## 変数管理

```bash
glab variable list                # プロジェクト変数一覧
glab variable list -g mygroup     # グループ変数一覧
glab variable get KEY             # 変数取得
glab variable set KEY value       # 変数設定
glab variable update KEY value    # 変数更新
glab variable export              # 変数エクスポート
glab variable delete KEY          # 変数削除
```

## 設定

```bash
glab config set editor vim        # エディタ設定
glab config set host gitlab.example.com  # デフォルトホスト
glab config set display_hyperlinks true  # ハイパーリンク表示
glab config get editor            # 設定値取得
glab config edit                  # 設定ファイルを開く
```

## グローバルフラグ

| フラグ | 説明 |
|--------|------|
| `-R OWNER/REPO` | 別リポジトリを指定 |
| `--web` / `-w` | ブラウザで開く |
| `--help` / `-h` | ヘルプ表示 |

## API 直接呼び出し

```bash
glab api projects/:id             # GET リクエスト
glab api projects/:id/issues -X POST -f title="Bug"  # POST リクエスト
glab api graphql -f query='{ currentUser { name } }'  # GraphQL
```

## よくある操作

| 目的 | コマンド |
|------|----------|
| MR を作成してレビュー依頼 | `glab mr create --fill --reviewer user1` |
| 自分担当の MR 一覧 | `glab mr list --assignee @me` |
| CI の状態確認 | `glab ci status` |
| ジョブログを追跡 | `glab ci trace <job-id>` |
| issue から MR を作成 | `glab mr for <issue-id>` |
| パイプライン手動実行 | `glab ci run` |
| リリース作成 | `glab release create <tag> <files>` |

## gh との主な違い

| 操作 | gh (GitHub) | glab (GitLab) |
|------|-------------|---------------|
| PR/MR 作成 | `gh pr create` | `glab mr create` |
| PR/MR 一覧 | `gh pr list` | `glab mr list` |
| CI 確認 | `gh run list` | `glab ci list` |
| リポジトリ指定 | `-R owner/repo` | `-R owner/repo` |
