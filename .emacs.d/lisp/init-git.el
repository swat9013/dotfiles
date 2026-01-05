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

(provide 'init-git)
;;; init-git.el ends here
