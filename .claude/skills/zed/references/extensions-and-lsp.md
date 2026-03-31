# 拡張機能とLSP リファレンス

## TOC

- [§1 拡張機能の宣言的管理](#1-拡張機能の宣言的管理)
- [§2 LSP設定](#2-lsp設定)
- [§3 言語固有設定](#3-言語固有設定)
- [§4 データディレクトリ（git管理外）](#4-データディレクトリgit管理外)

---

## §1 拡張機能の宣言的管理

`settings.json` の `auto_install_extensions` に拡張機能IDをbooleanで列挙。Zed起動時に未インストールなら自動インストールされる。

```json
{
  "auto_install_extensions": {
    "tokyo-night": true,
    "html": true,
    "toml": true
  }
}
```

- `settings.json` をgit管理するだけで複数マシン間の拡張機能リストが同期される
- バイナリは `~/Library/Application Support/Zed/extensions/` に保存されgit管理不要
- UIからのインストール: `cmd-shift-x` — UIからインストールした拡張機能は自動で `auto_install_extensions` に追加されないため手動記載が必要
- 除外: キー削除またはfalseをセット（既インストール分は残る）

---

## §2 LSP設定

```json
{
  "lsp": {
    "<サーバー名>": {
      "initialization_options": { ... },
      "settings": { ... }
    }
  }
}
```

| キー | タイミング | 用途 |
|------|-----------|------|
| `initialization_options` | 起動時（1回） | サーバー起動時の固定設定 |
| `settings` | ランタイム | `workspace/didChangeConfiguration` 対応サーバー向け |

**rust-analyzer でclippy検査**:
```json
{
  "lsp": {
    "rust-analyzer": {
      "initialization_options": {
        "check": { "command": "clippy" }
      }
    }
  }
}
```

---

## §3 言語固有設定

```json
{
  "languages": {
    "Python": {
      "tab_size": 4,
      "language_servers": ["basedpyright", "!pylance", "..."]
    }
  }
}
```

`language_servers` 配列の記法:

| 記法 | 意味 |
|------|------|
| `"basedpyright"` | 明示的に使用 |
| `"!pylance"` | 除外 |
| `"..."` | その他デフォルトをすべて含む |

LSP全体の無効化: `"enable_language_server": false`

---

## §4 データディレクトリ（git管理外）

| パス（macOS） | パス（Linux） | 役割 |
|--------------|--------------|------|
| `~/Library/Application Support/Zed/extensions/` | `~/.local/share/zed/extensions/` | 拡張機能バイナリ |
| `~/Library/Application Support/Zed/languages/` | `~/.local/share/zed/languages/` | LSPサーバーバイナリ |
| `~/Library/Application Support/Zed/db/` | `~/.local/share/zed/db/` | ワークスペースDB |
| `~/Library/Logs/Zed/Zed.log` | `~/.local/share/zed/logs/Zed.log` | ログ |

git管理不要な理由: バイナリを含みリポジトリが肥大化する。`auto_install_extensions` で自動再取得できる。

**トラブル時の対処**:
- 起動しない → `db/` を移動して再起動（DB初期化）
- 拡張機能が壊れた → `extensions/` 内の該当ディレクトリを削除して再起動
- LSPが起動しない → `languages/` 内の該当サーバーを削除して再起動（再DL）
