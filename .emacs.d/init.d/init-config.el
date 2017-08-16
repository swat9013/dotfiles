;; Languageの設定
(set-language-environment "japanese")
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;スクロールの設定
(xterm-mouse-mode t)
;; (mouse-wheel-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 3);;３行ずつ画面がスクロール

;;対応する括弧内を表示
(show-paren-mode 1)

;;(setq show-paren-style 'expression)
(setq show-paren-style 'mixed)
(set-face-attribute 'show-paren-match-face nil
                    :background nil :foreground nil
                    :underline "#ffff00" :weight 'extra-bold)

;;マジックコメントを無効化
(setq ruby-insert-encoding-magic-comment nil)

;;オートインデント時にスペースでインデント
(setq-default indent-tabs-mode nil)

;; 指定行にジャンプするボタンを追加
(global-set-key "\C-x\C-g" 'goto-line)

;; ウィンドウの上部に現在の関数名を表示します
(which-function-mode 1)

;; リージョンの強調表示?
(transient-mark-mode t)

;;バックアップファイルを作らない
(setq backup-inhibited t)

;;起動時のメッセージスキップ
(setq inhibit-startup-message t)

;;削除のショートカットキー
(global-set-key "\C-h" 'backward-delete-char)

;;行末の空白を表示
(setq-default show-trailing-whitespace t)

;;; メニューバーを消す
(menu-bar-mode -1)



;;; 現在行を目立たせる
;;(global-hl-line-mode)

;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;;圧縮されたファイルも編集＆日本語infoの文字化け防止
(auto-compression-mode t)

;;"yes or no"を"y or n"にする
(fset 'yes-or-no-p 'y-or-n-p)

;;バックアップファイルを作らせない
(setq make-backup-files nil)

;;自動保存ファイルを作らせない
(setq auto-save-default nil)

;;選択範囲自動インデントのキーバインドを変更
(global-set-key (kbd "M-?") 'indent-region )

;;分割表示した時の移動を簡単に
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;;保存時に行末空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 全角スペースとタブを強調表示
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
;; (set-face-foreground 'whitespace-space "LightSlateGray")
;; (set-face-background 'whitespace-space "DarkSlateGray")
;; (set-face-foreground 'whitespace-tab "LightSlateGray")
;; (set-face-background 'whitespace-tab "DarkSlateGray")
(set-face-foreground 'whitespace-space "DarkGoldenrod1")
(set-face-background 'whitespace-space nil)
(set-face-bold-p 'whitespace-space t)
(set-face-foreground 'whitespace-tab "DarkOliveGreen1")
(set-face-background 'whitespace-tab nil)
(set-face-underline  'whitespace-tab t)

;;コメントアウトの色を変える
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")

;;; 画像ファイルを表示
(auto-image-file-mode t)

;;emacs serverの設定 現在はzshに記載
;; server start for emacs-client
;; (require 'server)
;; (unless (server-running-p)
;;  (server-start)
;; )


;;フルパスを表示
;; (set-default 'mode-line-buffer-identification
;;              '(buffer-file-name ("%f") ("%b")))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

;; (global-set-key  (kbd "ESC <down>") 'move-line-down)
;; (global-set-key  (kbd "ESC <up>") 'move-line-up)
;; iterm
(global-set-key  (kbd "M-[ b") 'move-line-down)
(global-set-key  (kbd "M-[ a") 'move-line-up)
