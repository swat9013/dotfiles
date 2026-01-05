;;; init-core.el --- Core Settings -*- lexical-binding: t; -*-

;;; Commentary:
;; 基本設定、エンコーディング、UI、カスタム関数

;;; Code:

;; ============================================================
;; 言語・エンコーディング設定
;; ============================================================
(set-language-environment "japanese")
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; ============================================================
;; 基本UI設定
;; ============================================================
(use-package emacs
  :straight nil
  :custom
  ;; 起動画面
  (inhibit-startup-screen t)
  (initial-scratch-message nil)

  ;; ダイアログ
  (use-dialog-box nil)
  (use-short-answers t)  ; yes/no → y/n

  ;; ベル
  (ring-bell-function 'ignore)

  ;; 編集
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (delete-selection-mode t)

  ;; バックアップ・自動保存 (no-littering が管理)
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)
  (backup-inhibited t)

  ;; スクロール
  (scroll-conservatively 101)
  (scroll-margin 3)
  (scroll-step 3)
  (scroll-preserve-screen-position t)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-follow-mouse t)

  ;; 行の扱い
  (kill-whole-line t)  ; C-k で行全体を削除
  (truncate-lines nil)

  :init
  ;; UIモード
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (blink-cursor-mode -1)
  (xterm-mouse-mode 1)

  ;; 表示モード
  (column-number-mode 1)
  (line-number-mode 1)
  (global-auto-revert-mode 1)
  (electric-pair-mode 1)
  (show-paren-mode 1)
  (transient-mark-mode 1)
  (auto-compression-mode 1)
  (auto-image-file-mode 1)
  (which-function-mode 1))

;; show-paren-mode 設定
(setq show-paren-style 'mixed)

;; ============================================================
;; 履歴・セッション管理
;; ============================================================
(use-package recentf
  :straight nil
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("\\.recentf" "COMMIT_EDITMSG"))
  :config
  (recentf-mode 1)
  ;; no-littering ディレクトリを除外
  (with-eval-after-load 'no-littering
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  ;; 自動保存
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list)))

(use-package savehist
  :straight nil
  :custom
  (history-length 100)
  :config
  (savehist-mode 1))

(use-package saveplace
  :straight nil
  :config
  (save-place-mode 1))

;; ============================================================
;; Whitespace (全角スペース・タブ可視化)
;; ============================================================
(use-package whitespace
  :straight nil
  :custom
  (whitespace-style '(tabs tab-mark spaces space-mark))
  (whitespace-space-regexp "\\(\x3000+\\)")
  (whitespace-display-mappings
   '((space-mark ?\x3000 [?\〼])
     (tab-mark   ?\t     [?\xBB ?\t])))
  :config
  (global-whitespace-mode 1)
  ;; フェイス設定
  (set-face-foreground 'whitespace-space "DarkGoldenrod1")
  (set-face-background 'whitespace-space nil)
  (set-face-bold-p 'whitespace-space t)
  (set-face-foreground 'whitespace-tab "DarkOliveGreen1")
  (set-face-background 'whitespace-tab nil)
  (set-face-underline 'whitespace-tab t))

;; 行末空白表示
(setq-default show-trailing-whitespace t)

;; コメント色
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")

;; ============================================================
;; 行末空白削除 (保存時)
;; ============================================================
(defvar delete-trailing-whitespace-before-save t
  "If non-nil, delete trailing whitespace before saving.")

(defun my/delete-trailing-whitespace ()
  "Delete trailing whitespace if enabled."
  (when delete-trailing-whitespace-before-save
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'my/delete-trailing-whitespace)

;; ============================================================
;; カスタム関数
;; ============================================================
(defun move-line-down ()
  "Move current line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  "Move current line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Revert buffer without confirmation.
With FORCE-REVERTING, revert even if modified."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))

(defun all-indent ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun electric-indent ()
  "Indent region if active, otherwise indent entire buffer."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (all-indent)))

;; ============================================================
;; prog-mode 設定
;; ============================================================
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(provide 'init-core)
;;; init-core.el ends here
