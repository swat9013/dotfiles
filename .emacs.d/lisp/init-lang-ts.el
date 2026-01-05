;;; init-lang-ts.el --- TypeScript Development -*- lexical-binding: t; -*-

;;; Commentary:
;; TypeScript/JavaScript 開発設定

;;; Code:

;; ============================================================
;; TypeScript (Tree-sitter)
;; ============================================================
(use-package typescript-ts-mode
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 2)
  :config
  ;; インデント設定
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (setq-local tab-width 2)
              (setq-local indent-tabs-mode nil))))

;; ============================================================
;; JavaScript (Tree-sitter)
;; ============================================================
(use-package js-ts-mode
  :straight nil
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :custom
  (js-indent-level 2)
  :config
  (add-hook 'js-ts-mode-hook
            (lambda ()
              (setq-local tab-width 2)
              (setq-local indent-tabs-mode nil))))

;; ============================================================
;; JSON (Tree-sitter)
;; ============================================================
(use-package json-ts-mode
  :straight nil
  :mode "\\.json\\'"
  :custom
  (json-ts-mode-indent-offset 2))

(provide 'init-lang-ts)
;;; init-lang-ts.el ends here
