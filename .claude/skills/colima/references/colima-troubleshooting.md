# Colima トラブルシューティング・ベストプラクティス

`config-and-operations.md` から分離。

---

## トラブルシューティング

### Docker接続エラー

```bash
# 方法1: コンテキスト切り替え
docker context use colima

# 方法2: DOCKER_HOST明示
export DOCKER_HOST="unix://${HOME}/.colima/default/docker.sock"
```

### ディスク容量不足

```bash
# VM内のトリム（即効）
colima ssh -- sudo fstrim -a

# ディスク拡張（増加のみ可能）
colima stop && colima start --disk 100
```

### ネットワーク問題（DNS解決失敗）

```bash
colima stop && colima start --dns 8.8.8.8
```

### 状態リセット（最終手段）

```bash
colima delete && colima start
```

### ログ確認

```bash
colima logs                           # リアルタイムログ
cat ~/.colima/default/lima-vm.log     # VMログファイル
```

---

## ベストプラクティス

### リソース配分

| リソース | 推奨値 |
|---|---|
| CPU | ホストの1/2〜2/3 |
| メモリ | 最低4GB、ビルド多い場合8GB |
| ディスク | 60GB以上（増加のみ可能なので余裕を持つ） |

### パフォーマンス

- Apple Siliconでは必ず `vz + virtiofs + rosetta` の組み合わせを使用
- `virtiofs` はデフォルトの `sshfs` より大幅に高速

### 運用

- 週1回程度の再起動でメモリリーク・断片化を解消
- 用途別プロファイルで環境分離（default/test/k8sなど）
- 定期的に `docker system prune` でイメージ・コンテナ整理
- ディスクはあとから増やせるが減らせない → 初期設定に余裕を

### dotfilesでの管理

`colima/colima.yaml` → `~/.colima/default/colima.yaml` へシンボリックリンク。設定変更はdotfilesリポジトリで行う。
