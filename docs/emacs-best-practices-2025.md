# Emacs 最新設定ベストプラクティス完全ガイドライン (2025年版)

本ガイドラインは、Emacs 29/30以降の最新機能を活用した設定のベストプラクティスを網羅的にまとめたものです。

---

## 目次

1. [設計原則](#1-設計原則)
2. [ファイル構成](#2-ファイル構成)
3. [起動時間の最適化](#3-起動時間の最適化)
4. [パッケージ管理](#4-パッケージ管理)
5. [必須のデフォルト設定](#5-必須のデフォルト設定)
6. [補完システム (Modern Completion Stack)](#6-補完システム-modern-completion-stack)
7. [LSP/IDE機能 (Eglot + Tree-sitter)](#7-lspide機能-eglot--tree-sitter)
8. [プロジェクト管理](#8-プロジェクト管理)
9. [バージョン管理 (Magit)](#9-バージョン管理-magit)
10. [Org-mode設定](#10-org-mode設定)
11. [UI/UXの改善](#11-uiuxの改善)
12. [推奨パッケージ一覧](#12-推奨パッケージ一覧)
13. [デバッグとトラブルシューティング](#13-デバッグとトラブルシューティング)
14. [サンプル設定（完全版）](#14-サンプル設定完全版)

---

## 1. 設計原則

### 1.1 基本方針

```
✅ 推奨事項
- Emacs組み込み機能を優先的に活用する（Emacs 29+の新機能）
- 遅延読み込み（lazy loading）を徹底する
- モジュラー構成で関心を分離する
- 設定の再現性を確保する

❌ 避けるべきこと
- 不要なパッケージの過剰インストール
- 起動時に全パッケージを読み込む
- ~/.emacsファイルの使用（init.elを使用する）
- customize-set-variableの乱用
```

### 1.2 Emacs 29以降の重要な変更点

| 機能 | 説明 |
|------|------|
| **use-package** | 組み込み（インストール不要） |
| **Eglot** | 組み込みLSPクライアント |
| **Tree-sitter** | 組み込み構文解析 |
| **Native Compilation** | ELisp→ネイティブコード |
| **SQLite** | 組み込みサポート |
| **long-line対応** | パフォーマンス改善 |

---

## 2. ファイル構成

### 2.1 推奨ディレクトリ構造

```
~/.emacs.d/
├── early-init.el      # 最速で読み込まれる設定
├── init.el            # メイン設定ファイル
├── custom.el          # customize変数用（自動生成）
├── lisp/              # 自作Elisp
│   ├── init-defaults.el
│   ├── init-completion.el
│   ├── init-lsp.el
│   ├── init-git.el
│   └── init-org.el
├── etc/               # 設定ファイル（no-littering対応）
├── var/               # 永続データ（no-littering対応）
└── tree-sitter/       # Tree-sitter文法ファイル
```

### 2.2 設定ファイルの分割パターン

```elisp
;; init.el - モジュールローダー
(dolist (module '("init-defaults.el"
                  "init-completion.el"
                  "init-lsp.el"
                  "init-git.el"
                  "init-org.el"))
  (load (expand-file-name module
                          (expand-file-name "lisp" user-emacs-directory))))
```

### 2.3 Literate Config（Org-mode方式）

```org
#+TITLE: My Emacs Configuration
#+PROPERTY: header-args:emacs-lisp :tangle init.el

* Package Management
#+begin_src emacs-lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
#+end_src
```

タングル：`M-x org-babel-tangle`

---

## 3. 起動時間の最適化

### 3.1 early-init.el（必須設定）

```elisp
;;; early-init.el -*- lexical-binding: t; -*-

;; GC閾値を一時的に上げて起動を高速化
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; package.elの自動初期化を無効化（straight.el使用時）
;; (setq package-enable-at-startup nil)

;; UI要素を早期に無効化（フラッシュ防止）
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; フレームサイズの自動調整を無効化
(setq frame-inhibit-implied-resize t)

;; Native Compilationの警告抑制
(setq native-comp-async-report-warnings-errors 'silent)

;; ネイティブコンパイルキャッシュの場所（Emacs 29+）
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))
```

### 3.2 init.el冒頭の最適化

```elisp
;;; init.el -*- lexical-binding: t; -*-

;; 起動時間計測
(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'my/display-startup-time)

;; 起動後にGC閾値を適正値に戻す
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.1)))

;; ファイル名ハンドラを一時的に無効化
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))
```

### 3.3 use-packageによる遅延読み込み

```elisp
;; 遅延読み込みの基本パターン
(use-package magit
  :commands (magit-status magit-blame)    ; コマンド呼び出しまで遅延
  :bind (("C-x g" . magit-status)))       ; キーバインド時も遅延

(use-package python
  :mode ("\\.py\\'" . python-mode)        ; ファイルオープン時に読み込み
  :hook (python-mode . eglot-ensure))     ; フック時に読み込み

(use-package projectile
  :defer 1                                ; 1秒後に読み込み
  :config
  (projectile-mode +1))
```

### 3.4 ベンチマーク

```elisp
;; benchmark-init-elで計測
(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; 結果表示: M-x benchmark-init/show-durations-tabulated
```

---

## 4. パッケージ管理

### 4.1 package.el + use-package（推奨：シンプル）

```elisp
;; パッケージアーカイブ設定
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;; Emacs 29+ではuse-packageは組み込み
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ; 自動インストール
```

### 4.2 straight.el + use-package（推奨：再現性重視）

```elisp
;; early-init.el
(setq package-enable-at-startup nil)

;; init.el
;; straight.elのブートストラップ
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-packageとの統合
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; バージョンロック
;; M-x straight-freeze-versions でロックファイル作成
```

### 4.3 パッケージ管理の比較

| 方式 | メリット | デメリット |
|------|----------|------------|
| **package.el** | シンプル、組み込み | 再現性が低い |
| **straight.el** | Git管理、再現性高い | 初回起動が遅い |
| **elpaca** | 非同期、高速 | 新しい、情報少ない |
| **Doom/Spacemacs** | 設定済み | 学習コスト |

---

## 5. 必須のデフォルト設定

### 5.1 基本設定

```elisp
(use-package emacs
  :custom
  ;; 文字エンコーディング
  (default-buffer-file-coding-system 'utf-8-unix)
  
  ;; UI設定
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (ring-bell-function 'ignore)
  (use-dialog-box nil)
  (use-short-answers t)  ; yes/no → y/n
  
  ;; 編集設定
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (delete-selection-mode t)
  
  ;; バックアップ設定
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  
  ;; スクロール設定
  (scroll-conservatively 101)
  (scroll-margin 3)
  (scroll-preserve-screen-position t)
  
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (column-number-mode 1)
  (global-auto-revert-mode 1)
  (electric-pair-mode 1)
  (show-paren-mode 1))
```

### 5.2 履歴とセッション管理

```elisp
;; 最近開いたファイル
(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

;; ミニバッファ履歴
(use-package savehist
  :custom
  (history-length 100)
  :config
  (savehist-mode 1))

;; カーソル位置の記憶
(use-package saveplace
  :config
  (save-place-mode 1))
```

### 5.3 no-littering（ファイル整理）

```elisp
(use-package no-littering
  :ensure t
  :config
  ;; customファイルの場所
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))
  
  ;; auto-saveファイル
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  
  ;; recentfから除外
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))
```

---

## 6. 補完システム (Modern Completion Stack)

### 6.1 推奨構成（2024-2025年）

```
┌─────────────────────────────────────────────────┐
│          Modern Emacs Completion Stack          │
├─────────────────────────────────────────────────┤
│  Minibuffer: Vertico + Orderless + Marginalia   │
│  In-buffer:  Corfu + Cape                       │
│  Actions:    Embark                             │
│  Search:     Consult                            │
└─────────────────────────────────────────────────┘
```

### 6.2 Vertico（ミニバッファ補完）

```elisp
(use-package vertico
  :ensure t
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  :init
  (vertico-mode 1)
  
  :config
  ;; 拡張機能
  (use-package vertico-directory
    :after vertico
    :ensure nil
    :bind (:map vertico-map
           ("RET" . vertico-directory-enter)
           ("DEL" . vertico-directory-delete-char)
           ("M-DEL" . vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))
```

### 6.3 Orderless（柔軟なマッチング）

```elisp
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion))))
  ;; Emacs 31対応
  (completion-pcm-leading-wildcard t))
```

### 6.4 Marginalia（注釈表示）

```elisp
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))
```

### 6.5 Consult（検索強化）

```elisp
(use-package consult
  :ensure t
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r"   . consult-ripgrep)
         ("M-s f"   . consult-find)
         ("M-y"     . consult-yank-pop))
  :custom
  (consult-narrow-key "<"))
```

### 6.6 Embark（コンテキストアクション）

```elisp
(use-package embark
  :ensure t
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))
```

### 6.7 Corfu（インバッファ補完）

```elisp
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :init
  (global-corfu-mode 1)
  
  :config
  ;; ポップアップ情報表示
  (use-package corfu-popupinfo
    :ensure nil
    :hook (corfu-mode . corfu-popupinfo-mode)))

;; Cape（補完ソース追加）
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword))
```

### 6.8 アイコン表示

```elisp
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
```

---

## 7. LSP/IDE機能 (Eglot + Tree-sitter)

### 7.1 Eglot（組み込みLSP）

```elisp
(use-package eglot
  :ensure nil  ; Emacs 29+は組み込み
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)  ; パフォーマンス向上
  (eglot-sync-connect nil)      ; 非同期接続
  
  :config
  ;; サーバー設定例
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"))))
```

### 7.2 Tree-sitter設定

```elisp
;; Tree-sitter文法のインストール設定
(setq treesit-language-source-alist
      '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (c          "https://github.com/tree-sitter/tree-sitter-c")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                    "master" "src")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "typescript/src")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

;; 一括インストール関数
(defun my/treesit-install-all-grammars ()
  "Install all Tree-sitter grammars."
  (interactive)
  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist)))

;; 自動モードリマップ（旧モード→Tree-sitterモード）
(setq major-mode-remap-alist
      '((bash-mode       . bash-ts-mode)
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (css-mode        . css-ts-mode)
        (js-mode         . js-ts-mode)
        (json-mode       . json-ts-mode)
        (python-mode     . python-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode       . yaml-ts-mode)))
```

### 7.3 treesit-auto（自動化）

```elisp
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode 1))
```

### 7.4 Eglot vs lsp-mode比較

| 項目 | Eglot | lsp-mode |
|------|-------|----------|
| **組み込み** | ✅ (Emacs 29+) | ❌ |
| **設定量** | 少ない | 多い |
| **機能** | 必要十分 | 豊富 |
| **パフォーマンス** | 軽量 | やや重い |
| **推奨** | 一般用途 | 高度なIDE機能が必要な場合 |

---

## 8. プロジェクト管理

### 8.1 project.el（組み込み）

```elisp
(use-package project
  :ensure nil
  :bind-keymap ("C-x p" . project-prefix-map)
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-vc-dir "VC-Dir")
     (project-eshell "Eshell")
     (magit-project-status "Magit" ?m))))
```

### 8.2 Projectile（高機能）

```elisp
(use-package projectile
  :ensure t
  :custom
  (projectile-indexing-method 'alien)  ; 高速インデックス
  (projectile-sort-order 'recentf)
  (projectile-enable-caching t)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  
  ;; Consult統合
  (use-package consult-projectile
    :ensure t
    :bind (:map projectile-command-map
           ("b" . consult-projectile-switch-to-buffer)
           ("f" . consult-projectile-find-file))))
```

### 8.3 project.el vs Projectile

| 項目 | project.el | Projectile |
|------|------------|------------|
| **組み込み** | ✅ | ❌ |
| **機能** | 基本的 | 豊富 |
| **カスタマイズ** | 限定的 | 柔軟 |
| **推奨** | シンプルな用途 | 複雑なプロジェクト |

---

## 9. バージョン管理 (Magit)

### 9.1 基本設定

```elisp
(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask))
```

### 9.2 Git関連の追加パッケージ

```elisp
;; 差分表示
(use-package diff-hl
  :ensure t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode 1))

;; Git履歴参照
(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine))

;; GitHub/GitLab統合
(use-package forge
  :ensure t
  :after magit)
```

---

## 10. Org-mode設定

### 10.1 基本設定

```elisp
(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  ;; 基本設定
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "inbox.org"))
  
  ;; 見た目
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-startup-folded 'content)
  
  ;; TODO設定
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANC(c)")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  
  ;; ソースコード
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0))
```

### 10.2 キャプチャテンプレート

```elisp
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "inbox.org" "Tasks")
         "* TODO %?\n  %U\n  %a")
        ("n" "Note" entry (file+headline "notes.org" "Notes")
         "* %?\n  %U")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %?\n  %U")))
```

### 10.3 org-modern（モダンな見た目）

```elisp
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))
```

---

## 11. UI/UXの改善

### 11.1 テーマ

```elisp
;; Modus Themes（組み込み、Emacs 28+）
(use-package modus-themes
  :ensure nil
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  :config
  (load-theme 'modus-vivendi t))

;; または Doom Themes
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))
```

### 11.2 モードライン

```elisp
;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 30)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  :init
  (doom-modeline-mode 1))
```

### 11.3 フォント設定

```elisp
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 140)

;; 日本語フォント
(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "Noto Sans CJK JP"))

;; 絵文字
(set-fontset-font t 'emoji
                  (font-spec :family "Noto Color Emoji"))
```

### 11.4 which-key

```elisp
(use-package which-key
  :ensure t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-add-column-padding 2)
  :init
  (which-key-mode 1))
```

### 11.5 行番号

```elisp
;; prog-modeでのみ行番号表示
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; 相対行番号（お好みで）
;; (setq display-line-numbers-type 'relative)
```

---

## 12. 推奨パッケージ一覧

### 12.1 必須級

| パッケージ | 用途 | 備考 |
|------------|------|------|
| **vertico** | ミニバッファ補完 | |
| **orderless** | マッチング | |
| **marginalia** | 注釈表示 | |
| **consult** | 検索強化 | |
| **corfu** | インバッファ補完 | |
| **embark** | アクション | |
| **magit** | Git操作 | |
| **which-key** | キーバインド表示 | |

### 12.2 推奨

| パッケージ | 用途 |
|------------|------|
| **treesit-auto** | Tree-sitter自動設定 |
| **no-littering** | ファイル整理 |
| **doom-modeline** | モードライン |
| **diff-hl** | Git差分表示 |
| **ace-window** | ウィンドウ切替 |
| **avy** | ジャンプ |
| **expand-region** | 範囲選択 |
| **multiple-cursors** | マルチカーソル |

### 12.3 言語別

| 言語 | パッケージ |
|------|------------|
| **Python** | python-mode, pyvenv |
| **JavaScript/TypeScript** | typescript-ts-mode |
| **Rust** | rust-ts-mode |
| **Go** | go-ts-mode |
| **Web** | web-mode |
| **Markdown** | markdown-mode |

---

## 13. デバッグとトラブルシューティング

### 13.1 起動時のデバッグ

```bash
# 最小構成で起動
emacs -Q

# デバッグモードで起動
emacs --debug-init

# プロファイリング
emacs --profile
```

### 13.2 パッケージの問題切り分け

```elisp
;; 特定パッケージを無効化してテスト
(use-package problematic-package
  :disabled t)
```

### 13.3 よくある問題と解決策

| 問題 | 原因 | 解決策 |
|------|------|--------|
| 起動が遅い | 遅延読み込みなし | use-packageの:defer, :commands活用 |
| パッケージエラー | バージョン不整合 | package-refresh-contents実行 |
| Tree-sitterエラー | 文法未インストール | treesit-install-language-grammar |
| LSP接続失敗 | サーバー未インストール | 各言語のLSPサーバーをインストール |

### 13.4 便利なデバッグコマンド

```elisp
M-x describe-variable  ; 変数の値確認
M-x describe-function  ; 関数の説明
M-x describe-key       ; キーバインド確認
M-x profiler-start     ; プロファイラ開始
M-x profiler-report    ; プロファイラ結果
```

---

## 14. サンプル設定（完全版）

### 14.1 ミニマル構成（~50行）

```elisp
;;; init.el -*- lexical-binding: t; -*-

;; パッケージ設定
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; 基本設定
(use-package emacs
  :custom
  (inhibit-startup-screen t)
  (make-backup-files nil)
  (use-short-answers t)
  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-auto-revert-mode 1)
  (electric-pair-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1))

;; 補完
(use-package vertico :init (vertico-mode 1))
(use-package orderless :custom (completion-styles '(orderless basic)))
(use-package marginalia :init (marginalia-mode 1))
(use-package consult :bind (("C-s" . consult-line) ("C-x b" . consult-buffer)))
(use-package corfu :custom (corfu-auto t) :init (global-corfu-mode 1))

;; Git
(use-package magit :bind ("C-x g" . magit-status))

;; テーマ
(load-theme 'modus-vivendi t)
```

### 14.2 フル構成の参考リンク

- [System Crafters - Emacs From Scratch](https://systemcrafters.net/emacs-from-scratch/)
- [Protesilaos Stavrou's Config](https://protesilaos.com/emacs/dotemacs)
- [Doom Emacs](https://github.com/doomemacs/doomemacs)
- [Phundrak's Emacs Config](https://config.phundrak.com/emacs/)

---

## 参考資料

### 公式ドキュメント
- [GNU Emacs Manual](https://www.gnu.org/software/emacs/manual/)
- [Emacs Lisp Reference](https://www.gnu.org/software/emacs/manual/elisp.html)

### コミュニティリソース
- [Mastering Emacs](https://www.masteringemacs.org/)
- [System Crafters](https://systemcrafters.net/)
- [r/emacs](https://reddit.com/r/emacs)
- [EmacsConf](https://emacsconf.org/)

### パッケージドキュメント
- [use-package](https://github.com/jwiegley/use-package)
- [Vertico/Corfu/Consult](https://github.com/minad)
- [Magit](https://magit.vc/)
- [Org-mode](https://orgmode.org/)

---

*最終更新: 2025年1月*
*対象バージョン: Emacs 29.x / 30.x*
