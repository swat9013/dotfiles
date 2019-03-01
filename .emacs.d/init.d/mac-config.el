;; (use-package helm-gtags
;;   :init
;;   (add-hook 'ruby-mode-hook (lambda () (helm-gtags-mode)))
;;   :config
;;   (setq helm-gtags-path-style 'root)
;;   (setq helm-gtags-auto-update t))
;; (bind-key* "M-." 'helm-gtags-find-tag)
;; (bind-key* "M-," 'helm-gtags-pop-stack)
;; (bind-key* "M-r" 'helm-gtags-find-rtag)

(use-package migemo
  :config
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-command "cmigemo")
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (migemo-init)
)

(use-package avy-migemo
  :config
  (avy-migemo-mode 1)
  (require 'avy-migemo-e.g.swiper))
