# Colima 設定・操作リファレンス

## TOC
1. [インストール](#1-インストール)
2. [基本操作（起動・停止・SSH）](#2-基本操作起動停止ssh)
3. [設定ファイル（場所・編集・変更不可設定）](#3-設定ファイル場所編集変更不可設定)
4. [ネットワーク・マウント設定](#4-ネットワークマウント設定)
5. [カスタマイズ](#5-カスタマイズ)
6. [プロファイル管理](#6-プロファイル管理)
7. [Kubernetes](#7-kubernetes)
8. [トラブルシューティング / ベストプラクティス](#8-トラブルシューティング--9-ベストプラクティス) → `colima-troubleshooting.md`

---

## 1. インストール

```bash
brew install colima                   # 安定版
brew install --HEAD colima            # 開発版
brew install docker docker-compose    # Docker CLI（別途必要）
```

代替: `sudo port install colima`（MacPorts）、`nix-env -iA nixpkgs.colima`（Nix）

---

## 2. 基本操作（起動・停止・SSH）

### ライフサイクル

```bash
colima start                          # デフォルト起動
colima stop
colima restart
colima status
colima delete                         # VM削除
```

### 起動オプション（主要）

| オプション | 説明 | 例 |
|---|---|---|
| `--cpu` | CPU数 | `--cpu 4` |
| `--memory` | メモリ(GiB) | `--memory 8` |
| `--disk` | ディスク(GiB) | `--disk 60` |
| `--arch` | アーキテクチャ | `--arch aarch64` |
| `--vm-type` | VMタイプ | `--vm-type vz` |
| `--vz-rosetta` | Rosetta有効化 | フラグのみ |
| `--mount-type` | マウント方式 | `--mount-type virtiofs` |
| `--kubernetes` | k8s有効化 | フラグのみ |

### SSH

```bash
colima ssh                            # VMへSSH接続
colima ssh -- <cmd>                   # VMでコマンド実行
```

---

## 3. 設定ファイル（場所・編集・変更不可設定）

### ファイル場所

- デフォルト: `~/.colima/default/colima.yaml`
- カスタムプロファイル: `~/.colima/<profile>/colima.yaml`
- dotfilesでの管理: `colima/colima.yaml` → `~/.colima/default/` へシンボリックリンク

### 編集方法

```bash
colima start --edit                   # 起動前にエディタ開く
colima template                       # デフォルトテンプレート表示・編集
```

### 変更不可設定（VMの再作成が必要）

| 設定キー | 変更方法 |
|---|---|
| `arch` | `colima delete && colima start --arch <new>` |
| `runtime` | 同上 |
| `vmType` | 同上 |
| `mountType` | 同上 |

変更可能な設定（再起動のみ）: `cpu`, `memory`, `disk`（増加のみ）

---

## 4. ネットワーク・マウント設定

### ネットワーク設定（colima.yaml）

```yaml
network:
  address: true             # VMへのIPアドレス割り当て（macOSのみ）
  mode: shared              # shared / bridged
  interface: en0            # bridgedモード用
  dns:
    - 8.8.8.8
  dnsHosts:
    example.internal: 192.168.1.10
  gatewayAddress: ""
```

### マウント設定

```yaml
mounts:
  - location: ~/projects
    writable: true
  - location: /tmp/colima
    writable: true
```

### プロビジョニング（colima.yaml）

`provision` 配列でVM起動時スクリプト実行。`mode`: `system` / `user` / `after-boot` / `ready`

---

## 5. カスタマイズ

### Apple Silicon最適化（推奨構成）

```bash
colima start \
  --arch aarch64 \
  --vm-type vz \
  --vz-rosetta \
  --mount-type virtiofs
```

効果: 起動速度・ファイルI/O・x86_64バイナリ互換性の向上

### DOCKER_HOST 設定

```bash
export DOCKER_HOST="unix://${HOME}/.colima/default/docker.sock"
```

`.zshrc`や`.zshenv`への追記が必要な場合（Docker Desktop未使用時）

### Docker Desktop との共存

```bash
docker context use colima             # Colimaに切り替え
docker context use desktop-linux      # Docker Desktopに切り替え
docker context ls                     # コンテキスト一覧
```

### インセキュアレジストリ（colima.yaml）

```yaml
docker:
  insecure-registries:
    - registry.internal:5000
```

---

## 6. プロファイル管理

各プロファイルは独立したVM環境。Docker コンテキストも自動作成される（`colima-<profile>`）。

```bash
colima start -p dev                   # devプロファイル起動
colima start -p k8s --kubernetes
colima list                           # 全プロファイル一覧
colima stop -p dev
colima delete -p dev
```

| プロファイル例 | 用途 |
|---|---|
| `default` | 日常開発 |
| `test` | CI再現・テスト環境 |
| `k8s` | Kubernetes開発 |

---

## 7. Kubernetes

現行dotfiles設定では `kubernetes.enabled: false`。参考のみ。

```yaml
kubernetes:
  enabled: true
  version: v1.33.4+k3s1
  port: 6443
  args: []
```

起動後 `~/.kube/config` に自動追記される。`kubectl` は別途インストール要。

---

## 8. トラブルシューティング / 9. ベストプラクティス

詳細は `colima-troubleshooting.md` を参照。
