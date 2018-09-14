(require 'package)
(setq package-archives
      '(
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
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
(require-package 'bind-key)
(require-package 'company)
(require-package 'csv-mode)
(require-package 'diminish)
(require-package 'dockerfile-mode)
(require-package 'flycheck)
(require-package 'flycheck-d-unittest)
(require-package 'flycheck-tip)
(require-package 'flymake-cursor)
(require-package 'git-gutter+)
(require-package 'gitconfig-mode)
(require-package 'goto-chg)
(require-package 'helm)
(require-package 'helm-ag)
(require-package 'helm-descbinds)
(require-package 'helm-git-grep)
(require-package 'helm-gtags)
(require-package 'helm-projectile)
(require-package 'js2-mode)
(require-package 'magit)
(require-package 'markdown-mode)
(require-package 'migemo)
(require-package 'nginx-mode)
(require-package 'powerline)
(require-package 'projectile-rails)
(require-package 'rainbow-delimiters)
(require-package 'rainbow-mode)
(require-package 'ruby-electric)
(require-package 'ruby-end)
(require-package 'sass-mode)
(require-package 'scss-mode)
(require-package 'slim-mode)
(require-package 'smart-compile)
(require-package 'smart-mode-line)
(require-package 'smartrep)
(require-package 'ssh-config-mode)
(require-package 'string-inflection)
(require-package 'undo-tree)
(require-package 'undohist)
(require-package 'use-package)
(require-package 'web-mode)
(require-package 'wgrep)
(require-package 'wgrep-ag)
(require-package 'wgrep-helm)
(require-package 'which-key)
(require-package 'yaml-mode)
(require-package 'yascroll)
(require-package 'yasnippet)
;; (require-package 'anything)
;; (require-package 'auto-complete)
;; (require-package 'auto-complete-clang)
;; (require-package 'point-undo)
;; (require-package 'ruby-block)
