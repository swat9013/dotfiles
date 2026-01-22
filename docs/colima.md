# Colima 設定ガイドライン

> macOS/Linux向け軽量コンテナランタイムの設定リファレンス＆ベストプラクティス集

## 目次

1. [概要](#1-概要)
2. [インストール](#2-インストール)
3. [基本操作](#3-基本操作)
4. [設定ファイル](#4-設定ファイル)
5. [カスタマイズ](#5-カスタマイズ)
6. [プロファイル管理](#6-プロファイル管理)
7. [Kubernetes](#7-kubernetes)
8. [トラブルシューティング](#8-トラブルシューティング)
9. [ベストプラクティス](#9-ベストプラクティス)

---

## 1. 概要

### 1.1 Colimaとは

Colima（Containers on Lima）は、macOSおよびLinux向けの軽量コンテナランタイム。Docker Desktopの代替として、最小限のセットアップでDocker、Containerd、Kubernetesを提供する。

**主な特徴:**
- Intel/Apple Silicon両対応
- シンプルなCLIインターフェース
- 自動ポートフォワーディング・ボリュームマウント
- 複数インスタンス（プロファイル）対応
- Docker Desktop比で大幅な省メモリ（約350MB vs 1.2GB+）

### 1.2 対応ランタイム

| ランタイム | 説明 |
|-----------|------|
| Docker | デフォルト。既存のDockerワークフローと互換 |
| Containerd | 軽量なコンテナランタイム |
| Incus | 仮想マシンとコンテナの統合管理 |

### 1.3 システム要件

- macOS 13（Ventura）以降
- または Linux
- Homebrew（推奨インストール方法）

---

## 2. インストール

### 2.1 Homebrew（推奨）

```bash
# 安定版
brew install colima

# 最新開発版
brew install --HEAD colima
```

### 2.2 その他のパッケージマネージャ

```bash
# MacPorts
sudo port install colima

# Nix
nix-env -iA nixpkgs.colima

# Mise
mise use -g colima@latest
```

### 2.3 Docker CLIのインストール

Colimaはコンテナランタイムのみ提供するため、Docker CLIは別途必要。

```bash
brew install docker docker-compose
```

---

## 3. 基本操作

### 3.1 起動・停止

```bash
# デフォルト設定で起動
colima start

# 停止
colima stop

# 再起動
colima restart

# 状態確認
colima status
```

### 3.2 起動オプション

```bash
# リソース指定
colima start --cpu 4 --memory 8 --disk 100

# アーキテクチャ指定（Apple Silicon）
colima start --arch aarch64

# x86_64エミュレーション（Rosetta使用）
colima start --arch aarch64 --vm-type vz --vz-rosetta

# Kubernetes有効化
colima start --kubernetes
```

### 3.3 SSH接続

```bash
# VMへSSH接続
colima ssh

# コマンド実行
colima ssh -- ls -la
```

---

## 4. 設定ファイル

### 4.1 設定ファイルの場所

```bash
# デフォルトプロファイル
~/.colima/default/colima.yaml

# カスタムプロファイル
~/.colima/<profile_name>/colima.yaml

# COLIMA_HOME環境変数で変更可能
export COLIMA_HOME=/path/to/colima
```

### 4.2 設定の編集方法

```bash
# 起動時にエディタで編集
colima start --edit

# テンプレート編集
colima template
```

### 4.3 主要設定項目

```yaml
# リソース設定
cpu: 4                    # CPUコア数（デフォルト: 2）
memory: 8                 # メモリGB（デフォルト: 2）
disk: 100                 # ディスクGB（デフォルト: 100）

# アーキテクチャ・ランタイム（作成後変更不可）
arch: aarch64             # x86_64, aarch64, host
runtime: docker           # docker, containerd

# 仮想マシンタイプ
vmType: vz                # qemu, vz, krunkit
                          # vz: macOS 13以降、高速
                          # qemu: クロスプラットフォーム

# マウント方式
mountType: virtiofs       # virtiofs, 9p, sshfs
                          # virtiofs: vz使用時推奨
                          # sshfs: qemu使用時デフォルト

# Rosetta（Apple Silicon + vz のみ）
rosetta: true             # x86_64バイナリの高速実行

# ホスト名
hostname: colima          # SSH接続時のホスト名

# SSH設定
sshConfig: true           # ~/.ssh/config自動更新
```

### 4.4 ネットワーク設定

```yaml
network:
  # VMへの到達可能IP割り当て（macOSのみ）
  address: true

  # ネットワークモード
  mode: shared            # shared, bridged

  # ブリッジモード用インターフェース
  interface: en0

  # カスタムDNS
  dns:
    - 8.8.8.8
    - 8.8.4.4

  # 内部ホスト解決
  dnsHosts:
    host.docker.internal: host.lima.internal

  # ゲートウェイ（最後は2）
  gatewayAddress: 192.168.5.2
```

### 4.5 マウント設定

```yaml
mounts:
  - location: ~/projects
    writable: true
  - location: /tmp/colima
    writable: true
```

### 4.6 プロビジョニング

```yaml
# 起動時に実行するスクリプト（冪等性必須）
provision:
  - mode: system
    script: |
      apt-get update
      apt-get install -y htop
  - mode: user
    script: |
      echo "alias ll='ls -la'" >> ~/.bashrc
```

---

## 5. カスタマイズ

### 5.1 Apple Silicon最適化

```bash
# 推奨設定
colima start \
  --arch aarch64 \
  --vm-type vz \
  --vz-rosetta \
  --mount-type virtiofs \
  --cpu 4 \
  --memory 8
```

### 5.2 Docker環境変数

```bash
# ~/.zshrc または ~/.bashrc
export DOCKER_HOST="unix://${HOME}/.colima/default/docker.sock"
```

### 5.3 Docker Desktopとの共存

```bash
# Colimaコンテキストに切り替え
docker context use colima

# Docker Desktopに戻す
docker context use default

# コンテキスト一覧
docker context ls
```

### 5.4 インセキュアレジストリ

```bash
# 起動時に設定
colima start --edit

# docker: insecure-registries に追加
docker:
  insecure-registries:
    - myregistry.local:5000
```

---

## 6. プロファイル管理

### 6.1 プロファイルの概念

プロファイルは独立したColima環境。異なるリソース設定やアーキテクチャを並行して使用可能。

### 6.2 プロファイル操作

```bash
# プロファイル指定で起動
colima start --profile dev
colima start -p intel --arch x86_64

# プロファイル一覧
colima list

# 特定プロファイルの操作
colima stop -p dev
colima delete -p dev

# Dockerコンテキスト（自動作成）
docker context use colima-dev
```

### 6.3 ユースケース例

```bash
# 開発用（高リソース）
colima start -p dev --cpu 4 --memory 8

# テスト用（x86_64エミュレーション）
colima start -p test --arch x86_64

# Kubernetes用
colima start -p k8s --kubernetes --cpu 4 --memory 6
```

---

## 7. Kubernetes

### 7.1 有効化

```bash
# 起動時に有効化
colima start --kubernetes

# バージョン指定
colima start --kubernetes --kubernetes-version v1.33.3+k3s1
```

### 7.2 設定ファイル

```yaml
kubernetes:
  enabled: true
  version: v1.33.3+k3s1
  port: 6443              # 0でランダム
  args:
    - --disable=traefik   # 追加のk3s引数
```

### 7.3 kubectl設定

```bash
# kubeconfigは自動設定
kubectl get nodes

# コンテキスト確認
kubectl config get-contexts
```

---

## 8. トラブルシューティング

### 8.1 Docker接続エラー

**症状:** `Cannot connect to the Docker daemon`

```bash
# 解決策1: 環境変数設定
export DOCKER_HOST="unix://${HOME}/.colima/default/docker.sock"

# 解決策2: コンテキスト切り替え
docker context use colima
```

### 8.2 ディスク容量不足

```bash
# 未使用領域の解放（v0.5.0以降は自動）
colima ssh -- sudo fstrim -a

# ディスク拡張（v0.5.3以降）
colima stop
colima start --disk 200
```

### 8.3 ネットワーク問題

```bash
# DNS解決失敗時
colima start --dns 8.8.8.8 --dns 8.8.4.4

# ホストへの接続
# コンテナ内から: host.docker.internal
```

### 8.4 状態リセット

```bash
# 完全リセット（データ削除）
colima delete
colima start
```

### 8.5 ログ確認

```bash
# Colimaログ
colima logs

# VMログ
cat ~/.colima/default/lima-vm.log
```

---

## 9. ベストプラクティス

### 9.1 リソース設定

- **CPU:** ホストの1/2〜2/3を目安に設定
- **メモリ:** 最低4GB推奨（ビルド作業多い場合は8GB）
- **ディスク:** 使用状況に応じて調整（増加のみ可能）

### 9.2 パフォーマンス最適化

```bash
# Apple Silicon推奨構成
colima start \
  --vm-type vz \
  --vz-rosetta \
  --mount-type virtiofs
```

- `vz`: macOS標準仮想化で高速
- `virtiofs`: ファイル共有が高速
- `rosetta`: x86_64コンテナが高速

### 9.3 運用Tips

1. **定期的な再起動:** 長期稼働時はメモリ解放のため週1回程度再起動
2. **プロファイル活用:** 用途別に分離（開発、テスト、K8s等）
3. **不要イメージ削除:** `docker system prune -a` で定期クリーンアップ

---

## 参考資料

- [GitHub - abiosoft/colima](https://github.com/abiosoft/colima)
- [Colima FAQ](https://github.com/abiosoft/colima/blob/main/docs/FAQ.md)
- [Colima設定ファイル例](https://github.com/abiosoft/colima/blob/main/embedded/defaults/colima.yaml)

---

*最終更新: 2026-01*
*Colima v0.9.1 対応*
