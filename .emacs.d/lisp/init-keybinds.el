;;; init-keybinds.el --- Key Bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; グローバルキーバインド設定

;;; Code:

;; ============================================================
;; 基本キーバインド
;; ============================================================
;; C-h を Backspace に (Emacs 標準では help)
(global-set-key (kbd "C-h") 'backward-delete-char)

;; 行ジャンプ
(global-set-key (kbd "C-x C-g") 'goto-line)

;; ファイル再読込
(global-set-key (kbd "C-c C-r") 'revert-buffer-no-confirm)

;; 全体インデント
(global-set-key (kbd "C-x C-i") 'electric-indent)

;; ============================================================
;; ウィンドウ操作
;; ============================================================
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;; ============================================================
;; 行移動
;; ============================================================
(global-set-key (kbd "M-[ b") 'move-line-down)
(global-set-key (kbd "M-[ a") 'move-line-up)

;; ============================================================
;; スクロール
;; ============================================================
(global-set-key (kbd "M-p") 'scroll-down-command)
(global-set-key (kbd "M-n") 'scroll-up-command)

;; ============================================================
;; バッファ切り替え
;; ============================================================
(global-set-key (kbd "M-}") 'next-buffer)
(global-set-key (kbd "M-{") 'previous-buffer)

(provide 'init-keybinds)
;;; init-keybinds.el ends here
