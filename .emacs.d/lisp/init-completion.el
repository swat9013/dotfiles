;;; init-completion.el --- Completion System -*- lexical-binding: t; -*-

;;; Commentary:
;; Vertico + Orderless + Marginalia + Consult（シンプル構成）

;;; Code:

;; ============================================================
;; バイトコンパイラ向け宣言
;; ============================================================
(defvar consult-buffer)
(defvar consult-recent-file)
(defvar xref-show-xrefs-function)
(defvar xref-show-definitions-function)
(declare-function vertico-mode "vertico")
(declare-function marginalia-mode "marginalia")
(declare-function consult-customize "consult")
(declare-function consult-xref "consult-xref")

;; ============================================================
;; Vertico (ミニバッファ補完UI)
;; ============================================================
(use-package vertico
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  :init
  (vertico-mode 1)
  :config
  ;; vertico-directory 拡張
  (use-package vertico-directory
    :straight nil
    :after vertico
    :bind (:map vertico-map
           ("RET" . vertico-directory-enter)
           ("DEL" . vertico-directory-delete-char)
           ("M-DEL" . vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; ============================================================
;; Orderless (柔軟なマッチング)
;; ============================================================
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  ;; Emacs 31対応
  (completion-pcm-leading-wildcard t))

;; ============================================================
;; Marginalia (注釈表示)
;; ============================================================
(use-package marginalia
  :init
  (marginalia-mode 1))

;; ============================================================
;; Consult (検索・ナビゲーション強化)
;; ============================================================
(use-package consult
  :bind (("C-s"     . consult-line)           ; isearch 置換
         ("C-x b"   . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ("C-x C-t" . consult-recent-file)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r"   . consult-ripgrep)
         ("M-s f"   . consult-find)
         ("C-c C-g" . consult-ripgrep))
  :custom
  (consult-narrow-key "<")
  :config
  ;; プレビュー設定
  (consult-customize
   consult-buffer consult-recent-file
   :preview-key "M-.")

  ;; xref 統合
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(provide 'init-completion)
;;; init-completion.el ends here
