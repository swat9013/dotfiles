---
name: obsidian-cli
description: |-
  Obsidian公式CLIによるVault操作・ノート管理・自動化を支援する知識ベーススキル。
  Use when「Obsidian」「obsidian CLI」「Vault操作」「デイリーノート」「obsidian://」。
user-invocable: false
---

# Obsidian CLI 知識ベース

## バージョン情報

現在のバージョン: `!`obsidian version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1 || echo "(未インストール)"``

記録バージョン: `1.12.7`

バージョンが異なる場合、ユーザーに通知。確認後にWebFetchでリリースノートを取得し、スキル内容を更新。

---

## このdotfilesの現行設定

| 項目 | 値 |
|------|----|
| 管理方法 | `brew install --cask obsidian`（`.Brewfile` に登録済み） |
| CLIバイナリ | `/opt/homebrew/bin/obsidian`（Brewが自動リンク、GUI有効化不要） |
| Vault | `obsidian-vault` @ `~/Documents/obsidian-vault` |

---

## アーキテクチャ（非自明な特性）

- **リモートコントロール型**: 実行中のObsidianアプリとIPC通信。Headlessではない
- Obsidian未起動時は**自動起動される**が、起動完了まで数秒かかる
- コマンド出力の先頭にローディングメッセージが混入する場合がある → パイプ処理時は `tail -n +N` やgrepで除去

---

## コマンド構文の非自明なポイント

コマンド一覧は `obsidian help`、個別詳細は `obsidian help <command>` で取得可能。以下は help に明示されない注意点:

- **`file=` vs `path=`**: `file=` はwikilink名解決（部分一致）、`path=` は完全パス。スクリプトでは `path=` を使うこと（`file=` は曖昧一致で意図しないファイルを操作するリスクあり）
- **共通オプションパターン**: 多くのコマンドに `format=json|tsv|csv`, `total`, `verbose`, `active` がある。スクリプト連携時は `format=json`
- **サブコマンドの `:` 区切り**: `daily:append`, `search:context`, `property:set` 等。親コマンドが一覧系、`:` 付きが操作系

---

## トラブルシューティング

| 症状 | 原因・対処 |
|------|---------|
| `The CLI is unable to find Obsidian` | Obsidianアプリが未起動。`open -a Obsidian --background` で起動 |
| `installer is out of date` 警告 | 手動インストール版のインストーラが古い。`brew install --cask obsidian` で解決 |
| コマンド出力にローディングメッセージ混入 | `.asar` ロード時のログ。出力パース時は先頭行を除去するか `format=json` でパース |
| `vault=` 省略時に別Vaultが操作される | 単一Vaultなら省略可だが、複数Vault時は常に明示 |

---

## 公式リファレンス

- CLI公式: https://help.obsidian.md/cli
- URIスキーム: https://help.obsidian.md/Extending+Obsidian/Obsidian+URI
