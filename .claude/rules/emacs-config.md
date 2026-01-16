---
paths: .emacs.d/**
---

# Emacs設定アーキテクチャ

## パッケージ管理

**straight.el + use-package** を採用（package.elではない）

**理由**:
- 再現性: パッケージバージョンを固定可能
- Git統合: パッケージをGitリポジトリから直接取得
- ロックファイル: `straight/versions/default.el` でバージョン管理

## ディレクトリ構成

```
.emacs.d/
├── init.el              # メインエントリポイント
├── early-init.el        # 起動前設定 (Emacs 27+)
├── lisp/                # モジュール化された設定
│   ├── init-*.el        # 機能別設定モジュール
└── straight/
    ├── repos/           # Gitリポジトリ (gitignore)
    ├── build/           # ビルド済みパッケージ (gitignore)
    └── versions/        # バージョンロックファイル
```

## 読み込み順序

1. **early-init.el** (Emacs 27+): パッケージシステム初期化前の設定
2. **init.el**: straight.el bootstrap → lisp/モジュール読み込み
3. **lisp/init-*.el**: 各モジュールを`require`で読み込み

## モジュール構成

| モジュール | 内容 |
|-----------|------|
| init-core.el | コア設定、UI改善 |
| init-completion.el | 補完 (Cape, Company, Consult) |
| init-corfu.el | Corfu UI補完 |
| init-lsp.el | LSP統合 |
| init-treesit.el | Tree-sitter統合 |
| init-lang-ruby.el | Ruby/Rails対応 |
| init-lang-ts.el | TypeScript/JavaScript |
| init-lang-web.el | Web開発 |
| init-git.el | Magit, forge |
| init-project.el | Projectile |
| init-ui.el | UIカスタマイズ |
| init-editing.el | 編集機能 |
| init-keybinds.el | キーバインド |
| init-modeline.el | モードライン |
| init-macos.el | macOS固有設定 |

## 重要な注意点

### gitignore対象

- `straight/repos/`: パッケージのGitリポジトリ（触らない）
- `straight/build/`: ビルド済みファイル（自動生成）
- `var/`, `etc/`: ランタイムデータ

### モジュール命名規則

- **init-core**: 基本設定
- **init-lang-***: プログラミング言語固有設定
- **init-***: 機能カテゴリ別設定

各モジュールは `(provide 'init-*)` で提供され、`init.el` から `(require 'init-*)` で読み込まれる。

## 起動最適化

### early-init.el

```elisp
;; GC閾値を一時的に上げて起動高速化
(setq gc-cons-threshold most-positive-fixnum)

;; UI要素を早期に無効化（フラッシュ防止）
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Native Compilation警告抑制
(setq native-comp-async-report-warnings-errors 'silent)
```

### init.el

```elisp
;; 起動後にGC閾値を適正値に戻す
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

;; ファイル名ハンドラを一時的に無効化
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))
```

## use-package 遅延読み込み

| キーワード | 用途 | 例 |
|-----------|------|-----|
| `:commands` | コマンド呼び出しまで遅延 | `(magit-status magit-blame)` |
| `:hook` | フック時に読み込み | `(python-mode . eglot-ensure)` |
| `:mode` | ファイルオープン時 | `("\\.py\\'" . python-mode)` |
| `:defer t` | 遅延読み込み有効化 | 明示的に遅延 |
| `:defer N` | N秒後に読み込み | バックグラウンド読み込み |

**原則**: `:ensure nil` は組み込みパッケージのみ（Emacs 29+のeglot等）

## straight.el バージョン管理

```bash
# バージョンをロック
M-x straight-freeze-versions

# ロックファイル
straight/versions/default.el  # git管理対象
```

## デバッグ

```bash
emacs -Q              # 最小構成で起動
emacs --debug-init    # デバッグモードで起動
```

```elisp
;; パッケージ別起動時間計測
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;; M-x benchmark-init/show-durations-tabulated
```
