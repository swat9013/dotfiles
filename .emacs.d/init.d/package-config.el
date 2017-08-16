(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package ssh-config-mode
  :mode ((".ssh/config\\'"       . ssh-config-mode)
         ("sshd?_config\\'"      . ssh-config-mode)
         ("known_hosts\\'"       . ssh-known-hosts-mode)
         ("authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :init
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode 1)
  (custom-set-variables
   '(anzu-deactivate-region t)
   '(anzu-mode-lighter "")
   '(anzu-search-threshold 100))
  :bind (("M-%" . anzu-query-replace)
         ("M-$" . anzu-query-replace-regexp)))

(use-package yascroll
  :diminish yascroll-bar-mode
  :config
  (global-yascroll-bar-mode 1))

(use-package auto-complete
  :diminish auto-complete-mode)

(use-package auto-complete-config
  :config
  (global-auto-complete-mode t))
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  (sml/setup))

(use-package ace-jump-mode
  :bind(("C-c SPC" . ace-jump-mode)))

(use-package rainbow-delimiters)
(use-package magit)

(use-package ido
  :config
  (ido-mode t))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (defalias 'do 'undo-tree-redo))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;;
;; helm projectile
;;
(use-package helm
  :diminish helm-mode
  :config
  (helm-mode 1)
  :bind(("C-x C-o" . helm-mini)))

(use-package helm-config)
(use-package helm-files)
(use-package helm-ag
  :config
  (setq helm-ag-base-command "rg --vimgrep --no-heading")
  (setq helm-ag-insert-at-point 'symbol)
  :bind(("M-s" . helm-ag)
        ("M-p" . helm-ag-pop-stack)
        ("C-M-s" . helm-ag-this-file)
        ("M-x" . helm-M-x)
        ("C-x C-f" . helm-find-files)))

(use-package projectile
  :diminish projectile-mode
  :init
  (defun helm-projectile-ag ()
    (interactive)
    (helm-ag (projectile-project-root)))
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  :bind(("C-c C-f" . helm-projectile-find-file)
        ("C-c C-g" . helm-projectile-ag)))

;;
;; Ruby on Rails
;;
(use-package robe :disabled t
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup))


(use-package ruby-block
  :config
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))

(use-package rinari :disabled t)

(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("[Ra]kefile$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("Berksfile$" . ruby-mode)
         ("\\.rabl$" . ruby-mode))
  :config
  (defun ruby-mode-set-encoding () nil)
  (setq ruby-deep-indent-paren-style nil)
  (defadvice ruby-indent-line (after unindent-closing-paren activate)
    (let ((column (current-column))
          indent offset)
      (save-excursion
        (back-to-indentation)
        (let ((state (syntax-ppss)))
          (setq offset (- column (current-column)))
          (when (and (eq (char-after) ?\))
                     (not (zerop (car state))))
            (goto-char (cadr state))
            (setq indent (current-indentation)))))
      (when indent
        (indent-line-to indent)
        (when (> offset 0) (forward-char offset))))))

;;
;; file mode
;;
(use-package web-mode
  :mode (("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.html?$"     . web-mode))
  :config
  (setq web-mode-html-offset   2)
  (setq web-ode-markup-indent-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2)
  (setq web-mode-extra-snippets
        '(("erb" . (("name" . ("beg" . "end"))))
          ("html" . (("name" . ("beg" . "end"))))
          ("php" . (("name" . ("beg" . "end"))
                    ("name" . ("beg" . "end"))))
          )))

(use-package shell-script-mode
  :mode (("\\.zsh" . shell-script-mode)))

(use-package js2-mode
  :mode (("\\.\\(js\\|json\\)$" . js2-mode)))


(use-package scss-mode
  :mode (("\\.scss$" . scss-mode))
  :config
  (setq scss-compile-at-save nil))

(use-package yaml-mode
  :mode (("¥¥.yml$" . yaml-mode)))

(use-package markdown-mode)
(use-package gitconfig-mode)

(use-package nginx-mode
  :mode (("nginx\\(.*\\).conf[^/]*$" . nginx-mode)))

;;
;; safe-diminish
;;
(defmacro safe-diminish (file mode &optional new-name)
  "https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))

(safe-diminish "abbrev" 'abbrev-mode)
(safe-diminish "autorevert" 'auto-revert-mode)
(safe-diminish "whitespace" 'global-whitespace-mode)
