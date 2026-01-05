;;; init-corfu.el --- In-buffer Completion -*- lexical-binding: t; -*-

;;; Commentary:
;; Corfu + Cape によるインバッファ補完（シンプル構成）

;;; Code:

;; ============================================================
;; Corfu (ポップアップ補完)
;; ============================================================
(use-package corfu
  :custom
  (corfu-auto t)              ; 自動補完
  (corfu-auto-delay 0.2)      ; 遅延 (秒)
  (corfu-auto-prefix 2)       ; 最小文字数
  (corfu-cycle t)             ; 循環
  (corfu-preselect 'prompt)   ; 最初の候補を選択しない
  (corfu-scroll-margin 5)
  :bind (:map corfu-map
         ("C-n" . corfu-next)
         ("C-p" . corfu-previous)
         ("C-f" . corfu-insert)
         ("TAB" . corfu-insert)
         ([tab] . corfu-insert)
         ("C-h" . nil))       ; C-h を Backspace として使用可能に
  :init
  (global-corfu-mode 1)
  :config
  ;; ターミナルサポート
  (unless (display-graphic-p)
    (use-package corfu-terminal
      :straight (:host codeberg :repo "akib/emacs-corfu-terminal")
      :config
      (corfu-terminal-mode 1))))

;; ============================================================
;; Cape (補完バックエンド拡張)
;; ============================================================
(use-package cape
  :init
  ;; completion-at-point-functions に追加
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  :config
  ;; dabbrev 設定
  (setq cape-dabbrev-min-length 3))

(provide 'init-corfu)
;;; init-corfu.el ends here
