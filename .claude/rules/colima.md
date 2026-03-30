---
paths: colima/**
---

# Colima設定ガイド

## 変更不可設定（要注意）

**VM作成後に変更できない設定**: `arch`, `runtime`, `vmType`, `mountType`

これらを変更するには `colima delete` → 再作成が必要（**データ消失**）。`colima.yaml` を編集する際は必ずこの制約を意識する。

## コマンド早見表

```bash
colima start                          # デフォルト設定で起動
colima start --edit                   # 起動前に設定を編集
colima stop                           # 停止
colima restart                        # 再起動
colima status                         # 状態確認
colima list                           # インスタンス一覧
colima ssh                            # VM内シェルに接続
colima logs                           # ログ表示
colima template                       # デフォルトテンプレート表示
colima version                        # バージョン確認
colima delete                         # インスタンス削除（データ消失）
```

## 設定ファイル

**場所**: `~/.colima/default/colima.yaml`（dotfiles: `colima/colima.yaml` → シンボリックリンク）

```yaml
# 形式: YAML
# 編集: colima start --edit または直接編集後 colima restart
cpu: 3           # CPUコア数（変更可）
memory: 4        # メモリGB（変更可）
disk: 30         # ディスクGB（増加のみ可）
arch: aarch64    # アーキテクチャ（変更不可）
runtime: docker  # ランタイム（変更不可）
vmType: vz       # VMタイプ（変更不可）
mountType: virtiofs  # マウントタイプ（変更不可）
```

## トラブルシューティング

**Docker接続エラー**: `export DOCKER_HOST="unix://${HOME}/.colima/default/docker.sock"` を設定するか `docker context use colima` で切り替え。

**起動失敗**: `colima logs` でエラー確認。解決しない場合は `colima delete && colima start` で完全リセット（データ消失に注意）。

**ディスク容量不足**: `colima ssh -- sudo fstrim -a` で解放。ディスクサイズは増加のみ可能（`colima stop && colima start --disk <値>`）。

---

詳細は `colima` スキルを参照。
