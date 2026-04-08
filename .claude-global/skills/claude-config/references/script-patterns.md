# スクリプトパターン詳細

Python スクリプト共通構造と自由度パターンの詳細。

## Python スクリプト共通構造

PEP 723 shebang パターン（`uv run --script` で依存を自己完結させる）:

```python
#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "some-package>=1,<2",
# ]
# ///
"""スクリプトの説明。

Usage:
    script.py <subcommand> [options]
"""

import argparse
import json
import sys

def main() -> None:
    parser = argparse.ArgumentParser()
    # サブコマンド・引数定義
    args = parser.parse_args()
    result = {"key": "value"}
    print(json.dumps(result, ensure_ascii=False))

if __name__ == "__main__":
    main()
```

規約:
- `argparse` でCLI引数を定義（位置引数・サブコマンド・オプション）
- `json.dumps` で構造化出力（SKILL.mdが `jq` 等で解析）
- 終端判定キーが必要な場合は JSON に `"gate": "PASS"` 等を含める
- `ensure_ascii=False` で日本語を可読出力

---

## 自由度パターン

**Railroading回避原則**: 情報と目的を与え、手順の柔軟性を保つ。過度に具体的な手順指定はClaudeの適応力を制限する。「必ずXの後にYを実行し、Zで終了する」より「目的はXの達成。通常はY→Zだが状況に応じて判断する」。

### 高自由度（テキストベースの指示）

複数アプローチが有効な場合。Claudeの判断に委ねる。

```markdown
## コードレビュー
1. コード構造と設計を分析
2. バグやエッジケースを検出
3. 可読性・保守性の改善を提案
4. プロジェクト慣習への準拠を確認
```

### 中自由度（パラメータ付きテンプレート）

推奨パターンがあり、カスタマイズも許容する場合。

````markdown
## レポート生成

以下のテンプレートを基に、必要に応じてカスタマイズ:

```python
def generate_report(data, format="markdown", include_charts=True):
    # データ処理
    # 指定フォーマットで出力
    # オプションで可視化を含む
```
````

### 低自由度（具体的スクリプト）

操作が壊れやすく、一貫性が重要な場合。

````markdown
## データベース移行

以下を正確に実行:

```bash
python scripts/migrate.py --verify --backup
```

コマンドの変更やフラグの追加は禁止。
````
