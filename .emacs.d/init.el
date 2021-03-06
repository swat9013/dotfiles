
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)
(setq load-path (append '("~/.emacs.d/init.d") load-path))

(load "config/common")
(load "install-packages")
(load "config/package")

(when (equal system-type 'darwin)
  (load "config/mac"))

(eval-when-compile  (require 'use-package))
(require 'diminish)    ;; if you use :diminish
(require 'bind-key)    ;; if you use any :bind variant
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.001)
(setq frame-background-mode 'dark)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-comment-tag ((t (:foreground "brightblue"))))
 '(diff-added ((t (:background "yellow" :foreground "black"))))
 '(diff-hunk-header ((t (:background "brightred" :foreground "black"))))
 '(diff-removed ((t (:background "color-22" :foreground "brightred"))))
 '(font-lock-function-name-face ((t (:foreground "color-33"))))
 '(helm-selection ((t (:background "brightblue" :distant-foreground "black")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 100)
 '(avy-migemo-function-names
   (quote
    (swiper--add-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-ignore-order :around ivy--regex-ignore-order-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full)))
 '(package-selected-packages
   (quote
    (logview evil-magit helm-robe helm flycheck auto-complete yasnippet yascroll yaml-mode web-mode use-package undohist undo-tree ssh-config-mode smart-mode-line smart-compile slim-mode scss-mode sass-mode ruby-end ruby-electric ruby-block robe rinari rainbow-mode rainbow-delimiters powerline point-undo nginx-mode markdown-mode magit js2-mode helm-projectile helm-ag gitconfig-mode git-gutter flymake-cursor flycheck-tip flycheck-d-unittest csv-mode auto-complete-clang atom-dark-theme anzu anything ace-jump-mode)))
 '(robe-completing-read-func (quote helm-robe-completing-read)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
