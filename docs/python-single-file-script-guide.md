# Python 1ファイルスクリプト設計ガイド

## 概要

PEP 723（2024年採択）により、Pythonスクリプト内に依存関係を直接埋め込めるようになった。requirements.txtや仮想環境の事前準備なしで、1ファイルで完結するスクリプトを作成・共有できる。

## PEP 723 基本構文

```python
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "requests<3",
#     "rich",
# ]
# ///
```

| 要素 | 説明 |
|------|------|
| `# /// script` | メタデータブロック開始 |
| `# ///` | メタデータブロック終了 |
| `requires-python` | 必要なPythonバージョン（PEP 440形式） |
| `dependencies` | 依存パッケージリスト（PEP 508形式） |
| `[tool.*]` | ツール固有設定（オプション） |

## 対応ツール

| ツール | 実行コマンド | 特徴 |
|--------|-------------|------|
| **uv** | `uv run script.py` | 高速、ロック対応、推奨 |
| pipx | `pipx run script.py` | 既存環境との統合容易 |
| hatch | `hatch run script.py` | プロジェクト管理ツール連携 |
| pdm | `pdm run script.py` | PEP 582対応 |

## 推奨スクリプト構造

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "requests<3",
#     "rich",
# ]
# ///
"""スクリプトの説明（--help時に表示）

Usage:
    ./script.py <arg>
    ./script.py --verbose
"""

import argparse
import sys

import requests
from rich.console import Console


def parse_args() -> argparse.Namespace:
    """コマンドライン引数をパース"""
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("target", help="対象")
    parser.add_argument("-v", "--verbose", action="store_true", help="詳細出力")
    return parser.parse_args()


def main() -> int:
    """メインエントリーポイント"""
    args = parse_args()
    console = Console()

    try:
        # メインロジック
        pass
    except Exception as e:
        console.print(f"[red]Error:[/red] {e}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
```

### 構造の順序

1. shebang行（直接実行用）
2. PEP 723 メタデータブロック
3. モジュールdocstring（使用方法記述）
4. import文（標準ライブラリ → サードパーティ）
5. 関数定義
6. `main()` 関数
7. `if __name__ == "__main__":` ブロック

## uvコマンド

### 依存関係の追加

```bash
# メタデータブロックを自動生成・更新
uv add --script example.py 'requests<3' 'rich'
```

### 実行

```bash
# 依存関係は自動インストール
uv run example.py

# 引数付き
uv run example.py --verbose target
```

### 直接実行

```bash
# 実行権限付与
chmod +x example.py

# shebangにより直接実行可能
./example.py
```

### 依存関係のロック

```bash
# ロックファイル生成（再現性確保）
uv lock --script example.py
# → example.py.lock が生成される
```

## 再現性の確保

### exclude-newer による日付制約

```python
# /// script
# requires-python = ">=3.11"
# dependencies = ["requests"]
#
# [tool.uv]
# exclude-newer = "2025-01-01T00:00:00Z"
# ///
```

指定日時以降にリリースされたパッケージを除外。CI/CDやチーム間で同一環境を再現する場合に有用。

## argparseパターン

### 基本

```python
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="説明")
    parser.add_argument("input", help="入力ファイル")
    parser.add_argument("-o", "--output", default="out.txt", help="出力先")
    parser.add_argument("-v", "--verbose", action="store_true")
    return parser.parse_args()
```

### サブコマンド

```python
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)

    # list サブコマンド
    list_parser = subparsers.add_parser("list", help="一覧表示")
    list_parser.add_argument("--all", action="store_true")

    # add サブコマンド
    add_parser = subparsers.add_parser("add", help="追加")
    add_parser.add_argument("name", help="名前")

    return parser.parse_args()
```

### 型付き引数

```python
parser.add_argument("--count", type=int, default=10)
parser.add_argument("--threshold", type=float, required=True)
parser.add_argument("--mode", choices=["fast", "slow"], default="fast")
```

## エラーハンドリング

```python
def main() -> int:
    args = parse_args()

    try:
        result = process(args)
    except FileNotFoundError as e:
        print(f"Error: File not found: {e.filename}", file=sys.stderr)
        return 1
    except requests.RequestException as e:
        print(f"Error: Network error: {e}", file=sys.stderr)
        return 2
    except KeyboardInterrupt:
        print("\nInterrupted", file=sys.stderr)
        return 130

    print(result)
    return 0
```

### 終了コード規則

| コード | 意味 |
|--------|------|
| 0 | 成功 |
| 1 | 一般的なエラー |
| 2 | コマンドライン引数エラー |
| 130 | Ctrl+C による中断 |

## 実践例

### APIクライアント

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["httpx", "rich"]
# ///
"""GitHub リポジトリ情報取得"""

import argparse
import sys

import httpx
from rich.console import Console
from rich.table import Table


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("repo", help="owner/repo形式")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    console = Console()

    try:
        resp = httpx.get(f"https://api.github.com/repos/{args.repo}", timeout=10)
        resp.raise_for_status()
        data = resp.json()
    except httpx.HTTPError as e:
        console.print(f"[red]Error:[/red] {e}", file=sys.stderr)
        return 1

    table = Table(title=data["full_name"])
    table.add_column("Metric")
    table.add_column("Value", justify="right")
    table.add_row("Stars", str(data["stargazers_count"]))
    table.add_row("Forks", str(data["forks_count"]))
    console.print(table)
    return 0


if __name__ == "__main__":
    sys.exit(main())
```

### ファイル処理

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["pandas", "typer"]
# ///
"""CSVファイルの統計情報を表示"""

from pathlib import Path

import pandas as pd
import typer


def main(
    input_file: Path = typer.Argument(..., help="入力CSV"),
    columns: list[str] = typer.Option(None, "--col", "-c", help="対象列"),
) -> None:
    df = pd.read_csv(input_file)

    if columns:
        df = df[columns]

    typer.echo(df.describe())


if __name__ == "__main__":
    typer.run(main)
```

## ベストプラクティス

| カテゴリ | 推奨事項 |
|---------|---------|
| 構造 | `parse_args()` と `main()` を分離（テスト容易性） |
| 型 | 型ヒント使用（IDE補完、静的解析） |
| エラー | 例外は具体的にキャッチ、適切な終了コード |
| 出力 | 通常出力は stdout、エラーは stderr |
| docstring | モジュールdocstringで使用方法を記述 |
| 依存関係 | 最小限に、バージョン制約を明示 |

## 注意事項

| 項目 | 説明 |
|------|------|
| IDE補完 | Pylanceは現時点でPEP 723を自動認識しない |
| セキュリティ | インターネットからのスクリプトは内容確認後に実行 |
| 開発時 | 大規模開発はpyproject.toml + パッケージ構造を推奨 |

## 参考資料

- [PEP 723 – Inline script metadata](https://peps.python.org/pep-0723/)
- [uv Running scripts](https://docs.astral.sh/uv/guides/scripts/)
- [Python argparse Tutorial](https://docs.python.org/3/howto/argparse.html)
- [PEP 257 – Docstring Conventions](https://peps.python.org/pep-0257/)
