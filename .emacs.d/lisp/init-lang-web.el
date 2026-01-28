;;; init-lang-web.el --- Web Development -*- lexical-binding: t; -*-

;;; Commentary:
;; HTML/CSS/Web 開発設定

;;; Code:

;; ============================================================
;; web-mode (ERB, PHP, テンプレート)
;; ============================================================
(use-package web-mode
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.jsp\\'"       . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.html?\\'"     . web-mode)
         ("\\.ejs\\'"       . web-mode)
         ("\\.vue\\'"       . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (setq-local tab-width 2)
              (setq-local indent-tabs-mode nil))))

;; ============================================================
;; CSS (Tree-sitter)
;; ============================================================
(use-package css-ts-mode
  :straight nil
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2))

;; ============================================================
;; SCSS
;; ============================================================
(use-package scss-mode
  :mode "\\.scss\\'"
  :custom
  (scss-compile-at-save nil))

;; ============================================================
;; Markdown
;; ============================================================
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mdc\\'" . markdown-mode))
  :custom
  ;; パフォーマンス最適化: 重い機能を無効化
  (markdown-enable-wiki-links nil)
  (markdown-fontify-code-blocks-natively nil)
  ;; char-displayable-p遅延回避
  (markdown-url-compose-char ?∞)
  (markdown-blockquote-display-char "▌")
  (markdown-hr-display-char ?─)
  (markdown-definition-display-char ?⁘)
  ;; Enterでリスト項目を継続
  (markdown-indent-on-enter 'indent-and-new-item)
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              ;; ターミナル環境でfont-lock/jit-lock最適化
              (when (not (display-graphic-p))
                (setq-local font-lock-maximum-decoration 2)
                (setq-local jit-lock-defer-time 0.1)
                (setq-local jit-lock-stealth-time 1.0)
                (setq-local jit-lock-chunk-size 500))
              ;; markdown では行末空白を削除しない (改行のため)
              (setq-local delete-trailing-whitespace-before-save nil)
              ;; TABでインデント、S-TABでアンインデント
              (local-set-key (kbd "TAB") 'tab-to-tab-stop)
              (local-set-key (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)
              ;; Enterでリスト継続（electric-indentは無効化）
              (electric-indent-local-mode -1)
              ;; GFMチェックボックス挿入
              (local-set-key (kbd "C-c C-t") 'markdown-insert-gfm-checkbox)
              ;; グローバルキーバインドを優先（markdown-mode標準を上書き）
              (local-set-key (kbd "C-c C-f") 'project-find-file)
              (local-set-key (kbd "C-c C-r") 'revert-buffer-no-confirm)
              (local-set-key (kbd "M-{") 'previous-buffer)
              (local-set-key (kbd "M-}") 'next-buffer)
              (local-set-key (kbd "M-p") 'scroll-down-command)
              (local-set-key (kbd "M-n") 'scroll-up-command))))

;; ============================================================
;; YAML (Tree-sitter)
;; ============================================================
(use-package yaml-ts-mode
  :straight nil
  :mode (("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)
         ("\\.dig\\'" . yaml-ts-mode)
         ("\\.yml\\.liquid\\'" . yaml-ts-mode)))

;; ============================================================
;; その他の設定ファイル
;; ============================================================
(use-package dockerfile-ts-mode
  :straight nil
  :mode "Dockerfile\\'")

;; gitconfig-mode (git-modes パッケージに含まれる)
(use-package git-modes
  :mode (("\\.gitconfig\\'" . gitconfig-mode)
         ("\\.git/config\\'" . gitconfig-mode)
         ("\\.gitignore\\'" . gitignore-mode)
         ("\\.gitattributes\\'" . gitattributes-mode)))

(use-package nginx-mode
  :mode "nginx\\(.*\\)\\.conf[^/]*\\'")

(use-package ssh-config-mode
  :mode (("\\.ssh/config\\'" . ssh-config-mode)
         ("sshd?_config\\'" . ssh-config-mode)
         ("known_hosts\\'" . ssh-known-hosts-mode)
         ("authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :hook (ssh-config-mode . turn-on-font-lock))

(provide 'init-lang-web)
;;; init-lang-web.el ends here
