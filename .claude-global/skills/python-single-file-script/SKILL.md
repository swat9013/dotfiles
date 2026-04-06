---
name: python-single-file-script
user-invocable: false
description: |-
  PEP 723インラインメタデータを用いたPythonスクリプト作成とuv run実行を支援する知識ベーススキル。
  Use when「PEP 723」「uv run」「Pythonスクリプト」「single-file script」「インラインメタデータ」。
---

# python-single-file-script 知識ベース

## バージョン情報

現在のバージョン: `!`uv --version 2>/dev/null || echo "(未インストール)"``

記録バージョン: `uv 0.6.12`

バージョンが異なる場合、ユーザーに通知。確認後にWebFetchでリリースノートを取得し、スキル内容を更新。

---

## 設計判断

| 判断項目 | 推奨 | 理由 |
|---------|------|------|
| shebang | `#!/usr/bin/env -S uv run --script` | `-S` フラグが複数引数を一つの文字列として渡す（Linux の execve 制約回避） |
| 構造順序 | shebang → PEP 723 → docstring → import → 関数 → main() → if __name__ | docstring を PEP 723 ブロック直後に置くと `--help` に自動表示される |
| parse_args() 分離 | main() から分離して独立関数化 | sys.argv を注入せず parse_args() を直接テストできる |
| 終了コード | 0=成功 / 1=一般エラー / 2=引数エラー / 130=Ctrl+C | 引数エラー時は 2（CLIツール標準）、Ctrl+C は 130（SIGINT 慣習）を使用 |
| テスト配置 | <500行 かつ テスト<10 → 同一ファイル / それ以上 → 分離 | 同一ファイルは共有容易・完結性が高い。分離は保守性・CI/CD統合・フィクスチャ活用に有利 |
| 依存関係の制約 | バージョン制約を明示（例: `"requests<3"`） | 無制約だと `uv run` 時に最新版が引かれ、再現性が失われる |

---

## Gotchas

| 問題 | 原因 | 対処 |
|------|------|------|
| pytest がスクリプト内依存を認識しない | pytest が PEP 723 メタデータを自動読み込みしない | `uv run --with pytest pytest script.py` の `--with` フラグで明示追加 |
| IDE 補完が効かない | Pylance が PEP 723 を現時点で未対応 | pyproject.toml + 仮想環境を別途作成してシンボリックリンク等で補完 |
| shebang が Linux で失敗 | Linux の execve は複数引数を1つにまとめる（`-S` なしでは `uv run --script` が1引数扱い） | `#!/usr/bin/env -S uv run --script` の `-S` フラグを必ず使う |
| 再実行時に依存が変わる | バージョン制約なしで resolve が変わる | `dependencies` にバージョン制約を明示 or `exclude-newer` を設定 |

---

## 詳細リファレンス

| ファイル | 内容 |
|---------|------|
| `references/pep723-and-uv.md` | PEP 723 構文・uv コマンド・exclude-newer・pytest 実行コマンドの詳細 |
