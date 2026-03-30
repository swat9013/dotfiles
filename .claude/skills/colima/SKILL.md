---
name: colima
description: Colimaコンテナランタイムの設定・操作・トラブルシューティングを支援する知識ベーススキル。Use when「Colima」「colima start」「colima.yaml」「Docker runtime」「VM設定」。
user-invocable: false
---

# Colima 知識ベース

## バージョン情報

現在のバージョン: `!`colima version``

記録バージョン: `0.9.1`

バージョンが異なる場合、ユーザーに通知。確認後にWebFetchでリリースノートを取得し、スキル内容を更新。

---

## このdotfilesの現行Colima設定サマリ

| 項目 | 値 |
|------|----|
| 設定ファイル | `colima/colima.yaml` → `~/.colima/default/colima.yaml` |
| cpu | 3 |
| memory | 4 GB |
| disk | 30 GB |
| arch | aarch64 ⚠ 変更不可 |
| runtime | docker ⚠ 変更不可 |
| vmType | vz ⚠ 変更不可 |
| mountType | virtiofs ⚠ 変更不可 |
| mountInotify | true（experimental） |
| rosetta | false |
| binfmt | true |
| kubernetes.enabled | false |
| kubernetes.version | v1.33.4+k3s1 |
| autoActivate | true |
| network.address | false |
| network.mode | shared |
| portForwarder | ssh |
| sshConfig | true |
| hostname | colima |
| docker | {}（デフォルト） |

> ⚠ `arch` / `runtime` / `vmType` / `mountType` は VM 作成後の変更不可。変更する場合は `colima delete` → 再作成が必要。

---

## 操作コマンド早見表

```bash
colima start              # VM起動
colima stop               # VM停止
colima restart            # VM再起動
colima status             # 状態確認
colima list               # プロファイル一覧
colima ssh                # VM内シェルへ接続
colima logs               # ログ表示
colima template           # デフォルトテンプレートを$EDITOR表示
colima start --edit       # 起動前に設定を編集して起動
colima version            # バージョン確認
```

---

## 主要設定カテゴリ概要

| カテゴリ | 代表設定 | 詳細 |
|---------|---------|------|
| リソース | `cpu`, `memory`, `disk` | references/config-and-operations.md §3 |
| VM種別（⚠変更不可） | `arch`, `runtime`, `vmType`, `mountType` | references/config-and-operations.md §3 |
| ネットワーク | `network.address`, `network.mode`, `portForwarder` | references/config-and-operations.md §4 |
| プロファイル | `colima start --profile <name>` で複数VM管理 | references/config-and-operations.md §6 |
| Kubernetes | `kubernetes.enabled`, `kubernetes.version` | references/config-and-operations.md §7 |

---

## トラブルシューティング要点

| 症状 | 原因・対処 |
|------|---------|
| Docker接続エラー | `colima status` で起動確認。未起動なら `colima start`。`DOCKER_HOST` 未設定の場合は `export DOCKER_HOST="unix://$HOME/.colima/default/docker.sock"` |
| 起動失敗 | `colima logs` でエラー確認。`colima delete` → `colima start` で再作成（⚠ データ消去） |
| ディスク容量不足 | `colima.yaml` の `disk` を増量後 `colima delete` → 再作成。縮小は不可 |
| マウント遅延（virtiofs） | `mountInotify: true` を確認。それでも遅い場合は `colima restart` を試す |
| Docker Desktop競合 | Docker Desktopと同時起動するとソケットが競合する。片方を停止し `DOCKER_HOST` を明示指定 |

---

## references案内

| ファイル | 内容 |
|---------|------|
| `references/config-and-operations.md` | 全設定項目・プロファイル管理・Kubernetes連携の詳細リファレンス |
