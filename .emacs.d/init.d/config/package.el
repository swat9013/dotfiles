(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; (use-package wgrep
;;   :ensure t)
;; (use-package wgrep-ag
;;   :ensure t)
;; (use-package wgrep-helm
;;   :ensure t)
;; (setq wgrep-enable-key "e")
;; (setq wgrep-auto-save-buffer t)
;; (setq wgrep-change-readonly-file t)

(use-package smartrep)
(smartrep-define-key
  global-map "C-c" '(("-" . 'goto-last-change)
                      ("+" . 'goto-last-change-reverse)))

(use-package auto-package-update)
(auto-package-update-maybe)

(use-package which-key)
(use-package company
  :init
  (eval-after-load 'company '(push 'company-robe company-backends))
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
  (define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fで候補を設定
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
  (define-key company-active-map (kbd "C-h") nil) ;;; C-hのドキュメント表示を無効
  ;; yasnippetとの連携
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
      (append (if (consp backend) backend (list backend))
        '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package flycheck
  :init
  (add-hook 'ruby-mode-hook 'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(idle-change mode-enabled new-line save)))

(use-package desktop
  :init
  (setq desktop-base-file-name ".emacs.desktop")
  (desktop-save-mode 1)
  (setq history-length 250)
  (setq desktop-files-not-to-save "")
  ;; 他のemacs processでdesktopを使用中の場合はconflictするので使わない。
  (setq desktop-save 'if-exists) ; save only if desktop file exists
  (setq desktop-load-locked-desktop nil) ; don't load if the desktop is locked.
  ;; don't save if not loaded.
  (add-hook 'desktop-not-loaded-hook '(lambda() (desktop-save-mode -1)))
  (defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :config
  (global-git-gutter+-mode 1))

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode 1)
  (custom-set-variables
    '(anzu-deactivate-region t)
    '(anzu-mode-lighter "")
    '(anzu-search-threshold 100)))
(bind-key* "M-%" 'anzu-query-replace)
(bind-key* "M-$" 'anzu-query-replace-regexp)

(use-package yascroll
  :diminish yascroll-bar-mode
  :config
  (global-yascroll-bar-mode 1))

;; (use-package auto-complete
;;   :diminish auto-complete-mode)

;; (use-package auto-complete-config
;;   :config
;;   (global-auto-complete-mode t))
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  (sml/setup))

(use-package ace-jump-mode
  :bind(("C-c SPC" . ace-jump-mode)))

(use-package rainbow-delimiters)
(use-package magit)
(bind-key* "C-x m" 'magit-status)

;; (use-package ido
;;   :config
;;   (ido-mode t))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (defalias 'do 'undo-tree-redo))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package string-inflection)

;;
;; helm projectile
;;

;; (use-package helm
;;   ;; :diminish helm-mode
;;   :config
;;   (ido-mode -1)
;;   (helm-mode 1)
;;   (setq helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match    t)
;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
;;   (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;;   (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;;   :bind(("C-x C-o" . helm-mini)
;;         ("M-y" . helm-show-kill-ring)
;;         ("M-x" . helm-M-x)
;;         ("C-x C-f" . helm-find-files)
;;         ("C-x C-t" . helm-recentf)))


;; (use-package helm-config)
;; (use-package helm-files)
;; ;; (use-package helm-descbinds
;; ;;   :config
;; ;;   (helm-descbinds-mode))
;; (use-package helm-ag
;;   :config
;;   (setq helm-ag-base-command "rg --vimgrep --no-heading")
;;   (setq helm-ag-insert-at-point 'symbol)
;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
;;   (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;;   :bind(("C-M-s" . helm-ag-this-file)))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'ivy))
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)
;; (defun helm-projectile-rg ()
;;   (interactive)
;;   (helm-ag (projectile-project-root))))
;; (bind-key* "C-c C-f" 'helm-projectile-find-file)
;; (bind-key* "C-c C-g" 'helm-projectile-rg)
(bind-key* "C-c C-t" 'projectile-recentf)
(bind-key* "C-c C-r" 'projectile-replace)

(use-package recentf
  :config
  (recentf-mode)
  (set-variable 'recentf-max-saved-items 100)
  (set-variable 'recentf-exclude '(".recentf"))
  (set-variable 'recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)))

;;
;; counsel
;;

(use-package smex
  :ensure t
  :config
  (setq smex-completion-method 'ivy))

(use-package swiper
  :ensure t
  :config
  (setq swiper-include-line-number-in-search t)
  )
(bind-key* "C-s" 'swiper)

(use-package counsel
  :ensure t
  :config
  (defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-height 20) ;; minibufferのサイズを拡大
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
    '((t . ivy--regex-plus)))
  (setq ivy-initial-inputs-alist nil)
  :custom
  (ivy-format-function 'ivy-format-function-arrow)
  (counsel-yank-pop-separator "\n-------\n"))
(bind-key* "M-x" 'counsel-M-x)
(bind-key* "C-x C-f" 'counsel-find-file)
(bind-key* "C-x C-t" 'counsel-recentf)
(bind-key* "M-y" 'counsel-yank-pop)

(use-package counsel-projectile)
(bind-key* "C-c C-f" 'counsel-projectile-find-file)
(bind-key* "C-c C-g" 'counsel-projectile-rg)
(bind-key* "C-c g" 'counsel-projectile-git-grep)
(bind-key* "C-x C-o" 'counsel-projectile-switch-to-buffer)

(use-package smart-jump :ensure t)
(bind-key* "M-d" 'smart-jump-go)

;;
;; Ruby on Rails
;;

;; (use-package robe
;;   :init
;;   (add-hook 'ruby-mode-hook 'robe-mode)
;;   ;; (custom-set-variables '(robe-completing-read-func 'helm-robe-completing-read))
;;   ;; (add-hook 'robe-mode-hook 'ac-robe-setup)
;;   )

(use-package symbol-overlay
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode)
  )

;;(use-package ruby-block
;;  :config
;;  (ruby-block-mode t)
;;  (setq ruby-block-highlight-toggle t))

(use-package rinari :disabled t)

(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
          ("Capfile$" . ruby-mode)
          ("Gemfile$" . ruby-mode)
          ("[Ra]kefile$" . ruby-mode)
          ("\\.rake$" . ruby-mode)
          ("Berksfile$" . ruby-mode)
          ("Schemafile$" . ruby-mode)
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

(use-package projectile-rails
  :diminish projectile-rails-global-mode
  :config
  (projectile-rails-global-mode))

;;
;; typescript
;;
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

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
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0)
  (add-hook 'web-mode-hook
    (lambda () (set-variable 'tab-width 2))))

(use-package shell-script-mode
  :mode (("\\.zsh" . shell-script-mode)))

(use-package js2-mode
  :mode (("\\.\\(js\\|json\\)$" . js2-mode))
  :config
  (setq my-js-mode-indent-num 2)
  (setq js2-basic-offset my-js-mode-indent-num)
  (setq js-switch-indent-offset my-js-mode-indent-num)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override nil))

(use-package scss-mode
  :mode (("\\.scss$" . scss-mode))
  :config
  (setq scss-compile-at-save nil))

(use-package yaml-mode
  :mode (("¥¥.yml$" . yaml-mode)
          ("\\.dig" . yaml-mode)
          ("\\.yml.liquid" . yaml-mode)))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook '(lambda () (set (make-local-variable 'delete-trailing-whitespece-before-save) nil))))

(use-package gitconfig-mode)

(use-package nginx-mode
  :mode (("nginx\\(.*\\).conf[^/]*$" . nginx-mode)))

(use-package ssh-config-mode
  :mode ((".ssh/config\\'"       . ssh-config-mode)
          ("sshd?_config\\'"      . ssh-config-mode)
          ("known_hosts\\'"       . ssh-known-hosts-mode)
          ("authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :init
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package csharp-mode
  :mode (("\\.cs" . csharp-mode)))

(use-package typescript-mode
  :mode (("\\.ts" . typescript-mode)))

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
