;;; init-macos.el --- macOS Specific Settings -*- lexical-binding: t; -*-

;;; Commentary:
;; macOS 固有の設定

;;; Code:

;; ============================================================
;; exec-path-from-shell (PATH 継承)
;; ============================================================
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; ============================================================
;; クリップボード連携
;; ============================================================
(defun copy-to-osx ()
  "Copy region to macOS clipboard using pbcopy."
  (interactive)
  (when (region-active-p)
    (call-process-region (region-beginning) (region-end) "pbcopy")
    (setq deactivate-mark t)
    (message "Copied to clipboard")))

(global-set-key (kbd "C-c y") 'copy-to-osx)

;; ============================================================
;; macOS キーボード設定
;; ============================================================
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-control-modifier 'control)

;; fn キー設定
(setq ns-function-modifier 'hyper)

;; ============================================================
;; macOS 特有の挙動調整
;; ============================================================
;; スムーズスクロール
(setq scroll-conservatively 101)

;; フルスクリーン設定
(setq ns-use-native-fullscreen t)

;; タイトルバーにファイルパスを表示
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; ============================================================
;; Trash 対応
;; ============================================================
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

(provide 'init-macos)
;;; init-macos.el ends here
