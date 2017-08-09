(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'cl)

(defvar installing-package-list
  '(
    ;; cl-lib ;;emacs23用
    ;; ac-js2
    ;; ack-and-a-half
    ;; ag
    anzu
    ace-jump-mode
    atom-dark-theme
    anything
    ;; anything-complete
    ;; anything-config
    ;; anything-match-plugin
    ;; anything-obsolete
    ;; anything-show-completion
    auto-complete
    auto-complete-clang
    ;; auto-async-byte-compile
    ;; benchmark-init
    ;; coffee-mode
    csv-mode
    ;; d-mode
    ;; direx
    ;; diminish
    ;; editorconfig
    ;; exec-path-from-shell
    ;; expand-region
    ;; feature-mode
    flycheck
    flycheck-d-unittest
    flycheck-tip
    flymake-cursor
    git-gutter
    ;; git-gutter-fringe
    gitconfig-mode
    ;; haml-mode
    helm
    helm-ag
    helm-projectile
    ;; helm-ls-git
    ;; hlinum
    ;; jade-mode
    js2-mode
    magit
    markdown-mode
    ;; maxframe
    ;; minor-mode-hack
    ;; move-text
    ;; multiple-cursors
    nginx-mode
    point-undo
    ;; popwin
    powerline
    ;; projectile
    ;; projectile-rails
    rainbow-delimiters
    rainbow-mode
    ;; rbenv
    ;; recentf-ext
    ;; region-bindings-mode
    rinari
    ruby-block
    ruby-end
    ruby-electric
    robe
    sass-mode
    scss-mode
    ;; sequential-command
    ;; smartrep
    slim-mode
    smart-compile
    smart-mode-line
    ;; smartparens
    ;; smooth-scroll
    ;; smooth-scrolling
    ssh-config-mode
    ;; typescript
    ;; tabbar
    ;; textmate
    undo-tree
    undohist
    ;; volatile-highlights
    yascroll
    yasnippet
    yaml-mode
    web-mode
    ;; zenburn-theme
    ))

(defvar package-urls
  '(
    ;; 1ファイルのelispしか管理できません
    ;; パッケージ名はファイル名の.elより前の部分になります
    ;; "https://raw.githubusercontent.com/mooz/js2-mode/emacs23/js2-mode.el"
    ;; "http://cx4a.org/pub/undohist.el"
    )
  )


(defun package-install-from-url (url)
  "URLを指定してパッケージをインストールする"
  (interactive "sURL: ")
  (let ((file (and (string-match "\\([a-z0-9-]+\\)\\.el" url) (match-string-no-properties 1 url))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (delete-region (point-min) (search-forward "\n\n"))
      (goto-char (point-min))
      (setq info (cond ((condition-case nil (package-buffer-info) (error nil)))
                       ((re-search-forward "[$]Id: .+,v \\([0-9.]+\\) .*[$]" nil t)
                        (vector file nil (concat "[my:package-install-from-url]") (match-string-no-properties 1) ""))
                       (t (vector file nil (concat file "[my:package-install-from-url]") (format-time-string "%Y%m%d") ""))))
      (package-install-from-buffer info 'single)
      (kill-buffer)
      )))
(defun package-url-installed-p (url)
  "指定されたURLに対応するパッケージがインストールされているか調べる"
  (interactive "sURL: ")
  (let ((pkg-name (and (string-match "\\([a-z0-9-]+\\)\\.el" url) (match-string-no-properties 1 url))))
    (package-installed-p (intern pkg-name))))


(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

(let ((urls (loop for url in package-urls
                  unless (package-url-installed-p url)
                  collect url)))
  (dolist (url urls)
    (package-install-from-url url)))

(provide 'init-packages)
