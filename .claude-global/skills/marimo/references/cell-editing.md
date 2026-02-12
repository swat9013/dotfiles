# marimo セル編集パターン

Claude Code が marimo の .py ファイルを正確に編集するためのリファレンス。

## 完全なファイルテンプレート

```python
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "marimo",
#     "pandas",
#     "plotly",
# ]
# ///

import marimo

__generated_with = "0.11.17"
app = marimo.App(width="medium")


@app.cell
def __():
    import pandas as pd
    import marimo as mo
    return (mo, pd)


@app.cell
def __(mo):
    slider = mo.ui.slider(0, 100, value=50, label="閾値")
    slider
    return (slider,)


@app.cell
def __(pd, slider):
    df = pd.DataFrame({"value": range(100)})
    filtered = df[df["value"] > slider.value]
    filtered
    return (df, filtered)


if __name__ == "__main__":
    app.run()
```

## セル追加パターン

### 新しいセルの挿入位置

`@app.cell` デコレータの前に空行2行。`if __name__` の直前に追加するのが安全。

```python
# Edit tool で挿入する場合の old_string:
if __name__ == "__main__":
    app.run()

# new_string（新セルを末尾に追加）:
@app.cell
def __(filtered, mo):
    mo.md(f"フィルタ後: **{len(filtered)}** 件")
    return ()


if __name__ == "__main__":
    app.run()
```

### セル追加チェックリスト

1. `@app.cell` デコレータを付ける
2. 参照する変数を関数引数に列挙
3. 定義する変数を return タプルに列挙（末尾カンマ必須）
4. 何も定義しない場合は `return ()`
5. 前のセルとの間に空行2行
6. 表示したい値はセルの最後の式として記述

## return 文のパターン

```python
# 変数を1つ定義（末尾カンマでタプル化）
return (x,)

# 変数を複数定義
return (df, filtered)

# 変数を定義しない（表示のみ）
return ()

# import セル（marimo 自動生成準拠）
return mo, pd, np
```

## 変数参照の更新

セルが新しい変数を参照する場合、関数引数を更新する。

```python
# Before: mo のみ参照
@app.cell
def __(mo):
    mo.md("Hello")

# After: mo と df を参照
@app.cell
def __(df, mo):
    mo.md(f"行数: {len(df)}")
```

## PEP 723 インラインメタデータ

sandbox モードで依存関係をファイル内に記述。ファイル先頭（`import marimo` の前）に配置。

```python
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "marimo",
#     "pandas>=2.0",
#     "plotly",
# ]
# ///
```

パッケージ追加:
```bash
uv add --script notebook.py numpy
```

## marimo.App() のオプション

```python
app = marimo.App()                         # デフォルト
app = marimo.App(width="medium")           # 中幅（推奨）
app = marimo.App(width="full")             # 全幅
app = marimo.App(css_file="custom.css")    # カスタムCSS
```

## よくある編集ミス

| ミス | 正しい形 |
|------|---------|
| `return (x)` | `return (x,)` — カンマ必須 |
| `return x,` | OK（動作するが `return (x,)` が明示的） |
| 参照変数を引数に書き忘れ | `NameError` になる。引数に追加 |
| 定義変数を return に書き忘れ | 他セルから参照不可。return に追加 |
| セル間の空行が1行 | 2行空ける（トップレベル定義間は PEP 8 準拠） |
