---
name: marimo
description: marimo（リアクティブPythonノートブック）の操作ガイド。ファイル構造、セル編集パターン、リアクティブ設計、主要API、CLIコマンドを含む。「marimo」「マリモ」「リアクティブノートブック」「marimo edit」「marimo run」「marimoノートブック」と依頼された時に参照する。
user-invocable: false
---

# marimo

リアクティブ Python ノートブックの操作・編集ガイド。

## ファイル構造

marimo ノートブックは純粋な `.py` ファイル。Git フレンドリーで `python notebook.py` で直接実行可能。

```python
import marimo

__generated_with = "0.11.17"
app = marimo.App()


@app.cell
def __():
    import pandas as pd
    import marimo as mo
    return mo, pd


@app.cell
def __(mo):
    slider = mo.ui.slider(0, 100, value=50)
    slider
    return (slider,)


@app.cell
def __(slider):
    result = slider.value * 2
    return (result,)


if __name__ == "__main__":
    app.run()
```

### セル構造のルール

| 要素 | 説明 |
|------|------|
| `@app.cell` | セルデコレータ（必須） |
| 関数名 | `__()` が標準。意味のある名前も可 |
| 引数 | そのセルが**参照する**グローバル変数 |
| return | そのセルが**定義する**グローバル変数のタプル |
| 最後の式 | セルの出力（表示される値） |

### return 文のルール

| ケース | 書き方 | 理由 |
|--------|--------|------|
| 単一値 | `return (x,)` | カンマでタプル化（必須） |
| 複数値 | `return (x, y)` | 括弧で明示 |
| 定義なし | `return ()` | 空タプル |
| import セル | `return mo, pd` | marimo 自動生成準拠 |

### セル編集時の注意

- 変数を参照するなら関数引数に追加が必要
- 変数を定義するなら return タプルに追加が必要
- セル間に空行2行を入れる（PEP 8 準拠）
- 詳細: `~/.claude/skills/marimo/references/cell-editing.md`

## リアクティブ設計

### 核心ルール

**1変数1定義**: 各グローバル変数は1つのセルでのみ定義可能。

```python
# NG: 2つのセルで x を定義 → エラー
@app.cell
def __():
    x = 1
    return (x,)

@app.cell
def __():
    x = 2  # エラー: x は既に定義済み
    return (x,)
```

**ミューテーション非追跡**: `list.append()` や `df["col"] = ...` は他セルに伝播しない。

```python
# NG: 別セルでのミューテーション → リアクティブに伝播しない
@app.cell
def __():
    my_list = [1, 2, 3]
    return (my_list,)

@app.cell
def __(my_list):
    my_list.append(4)  # 他セルには反映されない

# OK: 同一セル内でミューテーション
@app.cell
def __():
    my_list = [1, 2, 3]
    my_list.append(4)
    return (my_list,)
```

**ローカル変数**: `_` プレフィックスでセルローカルに。

```python
@app.cell
def __():
    _tmp = expensive_computation()  # 他セルから見えない
    result = process(_tmp)
    return (result,)
```

### データ分析での設計パターン

```python
# パターン: データ加工パイプライン
# 各段階を別セルで定義し、依存関係を明確化

@app.cell
def __():
    import pandas as pd
    raw_df = pd.read_csv("data.csv")
    return (pd, raw_df)

@app.cell
def __(raw_df):
    # フィルタ・変換は新しい変数に代入（ミューテーション回避）
    cleaned_df = raw_df.dropna().query("value > 0")
    return (cleaned_df,)

@app.cell
def __(cleaned_df, mo):
    # インタラクティブテーブルで確認
    mo.ui.table(cleaned_df)
```

### mo.state() — 99%のケースで不要

使うべき稀なケース:
- フォーム送信履歴の蓄積
- 複数UIの同期（2つのスライダーを連動）

```python
get_val, set_val = mo.state(0)
slider_a = mo.ui.slider(0, 100, on_change=set_val, value=get_val())
slider_b = mo.ui.slider(0, 100, on_change=set_val, value=get_val())
```

## 主要 API

### 表示

```python
mo.md(f"# タイトル\n値: {variable}")    # Markdown（Python式埋め込み可）
mo.md(f"**{x}**") if condition else None  # 条件付き表示
mo.hstack([elem1, elem2])                # 水平レイアウト
mo.vstack([elem1, elem2])                # 垂直レイアウト
mo.accordion({"Section": content})        # アコーディオン
mo.tabs({"Tab1": content1})               # タブ
```

### UI 要素（`.value` で値取得、変更時に依存セル再実行）

```python
mo.ui.slider(start, stop, value=default, label="Label")
mo.ui.dropdown(options=["a", "b", "c"], value="a")
mo.ui.text(placeholder="入力...")
mo.ui.text_area(placeholder="複数行...")
mo.ui.checkbox(label="有効化")
mo.ui.number(start=0, stop=100, value=50)
mo.ui.date(value=datetime.date.today())
mo.ui.file(kind="area")                    # ファイルアップロード
mo.ui.button(label="実行", on_click=lambda _: None)
```

### データ分析向け

```python
mo.ui.table(df)                            # インタラクティブテーブル（選択・ソート・フィルタ）
mo.ui.dataframe(df)                        # DataFrame変換UI（no-code操作）
mo.sql(f"SELECT * FROM df WHERE col > {threshold}")  # SQL（dfを直接参照可）
```

### 制御フロー

```python
mo.stop(condition, mo.md("停止理由"))       # 条件付きセル停止
@mo.cache                                  # 結果キャッシュ（冪等な関数に）
```

## CLI クイックリファレンス

```bash
marimo edit notebook.py              # エディタで開く
marimo edit --sandbox notebook.py    # sandbox モード（uv で依存管理）
marimo run notebook.py               # 読み取り専用Webアプリとして実行
marimo convert input.ipynb -o out.py # Jupyter → marimo 変換
marimo export html notebook.py -o out.html  # HTML エクスポート
marimo check notebook.py             # lint・バリデーション
```

詳細: `~/.claude/skills/marimo/references/cli-commands.md`

## Gotchas

| 問題 | 原因 | 対処 |
|------|------|------|
| 変数再定義エラー | 複数セルで同名変数を定義 | セル統合 or `_` プレフィックスでローカル化 |
| UI変更が反映されない | 別セルでオブジェクトをミューテーション | 同一セル内で変更 or 新変数に代入 |
| セル実行順が意図と違う | DAG依存順で実行（位置順ではない） | 変数の依存関係を確認 |
| setup cell でエラー | 他セル変数への依存 | setup cell は自己完結させる |
| `mo.state` で無限ループ | 循環参照 | state は最小限に、通常のリアクティブフローを優先 |
