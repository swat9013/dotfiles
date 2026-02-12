# marimo CLI コマンドリファレンス

## edit — ノートブック編集

```bash
marimo edit notebook.py                  # エディタで開く
marimo edit                              # ファイルブラウザを開く
marimo edit --sandbox notebook.py        # sandbox モード（uv で依存管理）
marimo edit --port 8080                  # ポート指定
marimo edit --host 0.0.0.0              # バインドアドレス
marimo edit --headless                   # ブラウザを開かない
marimo edit --watch                      # ファイル変更時に自動リロード
marimo edit --no-token                   # 認証トークンなし
```

## run — Webアプリとして実行

```bash
marimo run notebook.py                   # 読み取り専用で実行
marimo run --sandbox notebook.py         # sandbox モード
marimo run --port 8080                   # ポート指定
marimo run --headless                    # ブラウザを開かない
marimo run --include-code                # ソースコードを表示可能にする
marimo run --base-url /app               # サブパスでデプロイ
```

## convert — 形式変換

```bash
# Jupyter → marimo
marimo convert notebook.ipynb -o notebook.py

# GitHub上のJupyterファイル
marimo convert https://github.com/.../notebook.ipynb -o notebook.py

# Markdown → marimo（{python} コードフェンス）
marimo convert document.md -o notebook.py

# py:percent → marimo
marimo convert script.py -o notebook.py
```

変換後の注意: セル間の変数ミューテーションが残っている場合がある。`marimo check --fix` で検出・修正。

## export — エクスポート

```bash
# HTML（実行結果付き）
marimo export html notebook.py -o output.html

# WASM HTML（ブラウザ単体で実行可能）
marimo export html-wasm notebook.py -o index.html

# Jupyter notebook
marimo export ipynb notebook.py -o output.ipynb

# Markdown
marimo export md notebook.py -o output.md

# PDF（nbformat/nbconvert 必要）
marimo export pdf notebook.py -o output.pdf

# フラットスクリプト（依存順でセルを並べた .py）
marimo export script notebook.py -o script.py

# 共通オプション
--watch                                  # ファイル変更時に再エクスポート
--sandbox                                # sandbox モードで実行
-f, --force                              # 上書き
```

## check — lint・バリデーション

```bash
marimo check notebook.py                 # 問題検出
marimo check --fix notebook.py           # 自動修正
```

検出項目: 変数再定義、循環依存、未使用 import 等。

## new — 新規作成

```bash
marimo new notebook.py                   # 空のノートブック
marimo new --ai "売上データを分析して可視化"  # AI 生成
```

## config — 設定管理

```bash
marimo config show                       # 現在の設定表示
marimo config describe                   # 全設定項目一覧
```

## データ分析の典型ワークフロー

```bash
# 1. sandbox で新規作成（依存管理自動）
marimo edit --sandbox analysis.py

# 2. エディタ内でパッケージインストール（自動でPEP 723メタデータに追加）

# 3. 結果をHTMLで共有
marimo export html analysis.py -o report.html

# 4. Jupyterユーザーへの共有
marimo export ipynb analysis.py -o analysis.ipynb
```

## uv 連携

```bash
# プロジェクト内でmarimo起動
uv run marimo edit notebook.py

# パッケージ追加（sandboxノートブック）
uv add --script notebook.py pandas plotly

# パッケージ削除
uv remove --script notebook.py pandas
```
