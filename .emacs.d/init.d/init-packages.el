(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
;; (package-refresh-contents)

(package-install 'ace-jump-mode)
(package-install 'anything)
(package-install 'anzu)
(package-install 'atom-dark-theme)
(package-install 'auto-complete)
(package-install 'auto-complete-clang)
(package-install 'csv-mode)
(package-install 'flycheck)
(package-install 'flycheck-d-unittest)
(package-install 'flycheck-tip)
(package-install 'flymake-cursor)
(package-install 'git-gutter+)
(package-install 'gitconfig-mode)
(package-install 'helm)
(package-install 'helm-ag)
(package-install 'helm-projectile)
(package-install 'helm-gtags)
(package-install 'js2-mode)
(package-install 'magit)
(package-install 'markdown-mode)
(package-install 'nginx-mode)
(package-install 'point-undo)
(package-install 'powerline)
(package-install 'projectile-rails)
(package-install 'rainbow-delimiters)
(package-install 'rainbow-mode)
(package-install 'ruby-block)
(package-install 'ruby-electric)
(package-install 'ruby-end)
(package-install 'sass-mode)
(package-install 'scss-mode)
(package-install 'slim-mode)
(package-install 'smart-compile)
(package-install 'smart-mode-line)
(package-install 'ssh-config-mode)
(package-install 'undo-tree)
(package-install 'undohist)
(package-install 'use-package)
(package-install 'web-mode)
(package-install 'yaml-mode)
(package-install 'yascroll)
(package-install 'yasnippet)
