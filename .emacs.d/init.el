(add-to-list 'load-path "~/.emacs.d/init.d")
;; (add-to-list 'load-path "~/.emacs.d/vendor.d")
(load "~/.emacs.d/init.d/init-config")

(require 'init-packages)


;;
;; モード追加
;;

;; (require 'anything)
;; (require 'anything-config)
;; (require 'anything-match-plugin)
;; (require 'anything-complete)
;; key config
;; (global-set-key (kbd "C-x b") 'anything-for-files)
;; (global-set-key (kbd "M-y") 'anything-show-kill-ring)
;; (global-set-key (kbd "C-c C-f") 'anything-filelist+)

;;csv-mode
(require 'csv-mode)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; git-gutter
(global-git-gutter-mode +1)

(require 'ssh-config-mode)
(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '(".ssh/config\\'"       . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'"      . ssh-config-mode))
(add-to-list 'auto-mode-alist '("known_hosts\\'"       . ssh-known-hosts-mode))
(add-to-list 'auto-mode-alist '("authorized_keys2?\\'" . ssh-authorized-keys-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;; anzu
(require 'anzu)
(global-anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 100))
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(require 'yascroll)
(global-yascroll-bar-mode 1)

;; theme
;;(load-theme 'atom-dark t)

;;; smooth-scroll
;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)

(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; (require 'projectile-rails)
;; (add-hook 'projectile-mode-hook 'projectile-rails-on)


(require 'flymake)
(add-hook 'find-file-hook 'flymake-find-file-hook)

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light)
(sml/setup)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; (defun add-keys-to-ace-jump-mode (prefix c &optional mode)
;;   (define-key global-map
;;     (read-kbd-macro (concat prefix (string c)))
;;     `(lambda ()
;;        (interactive)
;;        (funcall (if (eq ',mode 'word)
;;                     #'ace-jump-word-mode
;;                   #'ace-jump-char-mode) ,c))))

;; (loop for c from ?0 to ?9 do (add-keys-to-ace-jump-mode "H-" c 'word))
;; (loop for c from ?a to ?z do (add-keys-to-ace-jump-mode "H-" c 'word))

(require 'helm)
(helm-mode 1)
(define-key global-map (kbd "C-x C-o") 'helm-mini)

(require 'helm-config)
(require 'helm-files)
(require 'helm-ag)
(setq helm-ag-base-command "ag --nocolor --nogrou")
(global-set-key (kbd "M-s") 'helm-ag)
(global-set-key (kbd "M-p") 'helm-ag-pop-stack)
(global-set-key (kbd "C-M-s") 'helm-ag-this-file)

;;(require 'hlinum)
;;(hlinum-activate)
;;(global-linum-mode t)

(require 'rainbow-delimiters)
;;(require 'rainbow-mode)
(require 'markdown-mode)
(require 'gitconfig-mode)

;;(require 'magit)

;;webmodの設定
(require 'web-mode)
;;; emacs 23以下の互換
(when (< emacs-major-version 24)
  (defalias 'prog-mode 'fundamental-mode))
;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-markup-indent-offset   2)
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
          ))
  )
(add-hook 'web-mode-hook 'web-mode-hook)



(require 'ruby-mode)
;;ruby-mode勝手にcordingを入れないようにする
(defun ruby-mode-set-encoding () nil)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Ra]kefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))
;; indent
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
      (when (> offset 0) (forward-char offset)))))


;; (setq load-path (cons "~/.emacs.d/elisp" load-path))

(add-to-list 'auto-mode-alist '("\\.zsh" . shell-script-mode))


(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; smart-compile
(require 'smart-compile)
(define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
(define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))


(require 'scss-mode)
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   ;;   (set (make-local-variable 'scss-compile-at-save) nil)
   )
  )
(add-hook 'scss-mode-hook
          '(lambda() (scss-custom)))


;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)


;;;; Rinari
(add-to-list 'load-path "rinari")
(require 'rinari)

;;; rhtml-mode
;; (add-to-list 'load-path "rhtml")
;; (require 'rhtml-mode)
;; (add-hook 'rhtml-mode-hook
;;           (lambda () (rinari-launch)
;;             (set-face-background 'erb-face "black")
;;             ;;(set-face-underline-p 'erb-face t)
;;             (set-face-background 'erb-exec-face "black")
;;             ;;(set-face-underline-p 'erb-exec-face t)
;;             ))
;; (add-to-list 'auto-mode-alist '("\\.\\(erb\\|rhtml\\)$" . rhtml-mode))


;; yaml-mode
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("¥¥.yml$" . yaml-mode)))


;;undo-tree redoが出来る
(require 'undo-tree)
(global-undo-tree-mode)
(defalias 'redo 'undo-tree-redo)
;;(global-set-key (kbd "C-z") 'undo);; 【Ctrl+z】
;;(global-set-key (kbd "C-\\") 'redo)

;;;; el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))
;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;; (el-get 'sync)

;;;; cl-lib
;; (when (<= emacs-major-version 23)
;;   (el-get 'sync '(cl-lib)))


;; yasnippet
;; (add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1);; minor-modeもある
;; (define-key yas-minor-mode-map (kbd "SPC") 'yas/expand)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(yas-trigger-key "TAB"))

;;nginx-mode
(require 'nginx-mode)
(add-to-list 'auto-mode-alist '("nginx\\(.*\\).conf[^/]*$" . nginx-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
