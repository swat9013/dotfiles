;;; init-ui.el --- UI Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; テーマ、フォント設定（シンプル構成）

;;; Code:

;; ============================================================
;; Catppuccin テーマ (ghostty と統一)
;; ============================================================
;; ghostty: light:Catppuccin Latte, dark:Catppuccin Mocha
(use-package catppuccin-theme
  :custom
  ;; フレーバー: latte, frappe, macchiato, mocha
  (catppuccin-flavor 'mocha)  ; ダークテーマ
  (catppuccin-italic-comments t)
  (catppuccin-italic-blockquotes t)
  :config
  (load-theme 'catppuccin t))

;; システムのダーク/ライトモードに連動 (macOS)
(defun my/apply-theme-by-system-appearance ()
  "Apply Catppuccin theme based on system appearance."
  (let ((appearance (plist-get (mac-application-state) :appearance)))
    (setq catppuccin-flavor (if (string= appearance "dark") 'mocha 'latte))
    (catppuccin-reload)))

;; ============================================================
;; フォント設定 (GUI Emacs 用)
;; ============================================================
(when (display-graphic-p)
  ;; 英字フォント
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 140)

  ;; 日本語フォント
  (set-fontset-font t 'japanese-jisx0208
                    (font-spec :family "Noto Sans CJK JP"))

  ;; 絵文字
  (set-fontset-font t 'emoji
                    (font-spec :family "Noto Color Emoji")))

(provide 'init-ui)
;;; init-ui.el ends here
