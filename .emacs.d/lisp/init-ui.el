;;; init-ui.el --- UI Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; テーマ、フォント設定（シンプル構成）

;;; Code:

;; ============================================================
;; バイトコンパイラ向け宣言
;; ============================================================
(declare-function mac-application-state "macfns" nil t)
(declare-function set-fontset-font "fontset")

;; ============================================================
;; Tokyo Night テーマ (ghostty と統一)
;; ============================================================
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-tokyo-night t)
  (doom-themes-org-config))

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
