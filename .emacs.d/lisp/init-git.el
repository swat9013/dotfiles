;;; init-git.el --- Git Integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit によるGit操作（シンプル構成）

;;; Code:

;; ============================================================
;; Magit
;; ============================================================
(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-x m"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask))

;; ============================================================
;; diff-hl: Git差分を行単位で可視化
;; ============================================================
(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; ターミナルEmacs用（GUI不要の場合はマージンに表示）
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

(provide 'init-git)
;;; init-git.el ends here
