(require 'package)
(setq package-archives
  '(
    ;;  ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
    ;;  ("org" . "http://orgmode.org/elpa/")
    ;;  ("melpa" . "http://melpa.org/packages/")
    ;;  ("melpa-stable" . "http://stable.melpa.org/packages/")
  ))
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
  If NO-REFRESH is non-nil, the available package lists will not be
  re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
    t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(require-package 'ace-jump-mode)
(require-package 'anzu)
(require-package 'atom-dark-theme)
(require-package 'auto-package-update)
(require-package 'avy-migemo)
(require-package 'bind-key)
(require-package 'company)
(require-package 'counsel)
(require-package 'counsel-projectile)
(require-package 'diminish)
(require-package 'editorconfig)
(require-package 'flycheck)
(require-package 'flycheck-d-unittest)
(require-package 'flycheck-tip)
(require-package 'flymake-cursor)
(require-package 'git-gutter+)
(require-package 'goto-chg) ;;過去に編集した位置に戻る
(require-package 'magit)
(require-package 'migemo)
(require-package 'powerline)
(require-package 'projectile-rails)
(require-package 'rainbow-delimiters) ;;カッコの色をわかりやすくする
(require-package 'rainbow-mode) ;;色文字列を可視化
(require-package 'ruby-electric);;rubyのコード補助
(require-package 'ruby-end) ;;endを挿入してくれる
(require-package 'smart-jump) ;;定義ジャンプ
(require-package 'smart-mode-line) ;;Mode-Lineの視認性を変更
(require-package 'smartrep) ;;複数回実行が簡単になるキーバインドをせっていできる
(require-package 'smex) ;;曖昧検索
(require-package 'string-inflection) ;;文字の記法の変換
(require-package 'symbol-overlay)
(require-package 'tide)
(require-package 'undo-tree)
(require-package 'undohist)
(require-package 'use-package)
(require-package 'which-key) ;;キーバインドのチートシートを表示する
(require-package 'yascroll)
(require-package 'yasnippet)
;; (require-package 'anything)
;; (require-package 'auto-complete)
;; (require-package 'auto-complete-clang)
;; (require-package 'helm)
;; (require-package 'helm-ag)
;; (require-package 'helm-descbinds)
;; (require-package 'helm-git-grep)
;; (require-package 'helm-gtags)
;; (require-package 'helm-projectile)
;; (require-package 'helm-robe)
;; (require-package 'point-undo)
;; (require-package 'presentation) ;;文字の拡縮ができる
;; (require-package 'robe) ;; Rubyのコード補完
;; (require-package 'ruby-block)
;; (require-package 'smart-compile) ;;コンパイルを好きなキーバインドで実行できる
;; (require-package 'wgrep)
;; (require-package 'wgrep-ag)
;; (require-package 'wgrep-helm)

;; syntax
(require-package 'csv-mode)
(require-package 'dockerfile-mode)
(require-package 'gitconfig-mode)
(require-package 'js2-mode)
(require-package 'markdown-mode)
(require-package 'nginx-mode)
(require-package 'omnisharp)
(require-package 'sass-mode)
(require-package 'scss-mode)
(require-package 'slim-mode)
(require-package 'ssh-config-mode)
(require-package 'typescript-mode)
(require-package 'web-mode)
(require-package 'yaml-mode)
