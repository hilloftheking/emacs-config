;;; init.el --- Main config file for emacs

;;; Commentary:
;; My personal config, probably sucks

;;; Code:
;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package which-key
  :init
  (which-key-mode))
(use-package company
  :init
  (global-company-mode))
(use-package lsp-mode
  :init
  (progn
    (setq lsp-keymap-prefix "C-l")
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  :hook
  (c-mode-common . lsp))
(use-package lsp-ui)
(use-package gdscript-mode)
(use-package magit)
(use-package magithub)
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"))
(use-package flycheck
  :init
  (global-flycheck-mode))
(use-package clang-format
  :init
  (progn
    (add-hook
     'c-mode-common-hook
     (lambda ()
       (setq clang-format-fallback-style "llvm")))))
(use-package clang-format+
  :hook c-mode-common)
(use-package meson-mode)
(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))
(use-package gruvbox-theme)
(use-package multiple-cursors
  :config
  (define-key
    (current-global-map)
    (kbd "C-c c")
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "*") #'mc/mark-all-like-this)
      (define-key map (kbd "n") #'mc/mark-next-like-this)
      (define-key map (kbd "p") #'mc/mark-previous-like-this)
      (define-key map (kbd "e") #'mc/edit-lines)
      map)))
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; Use cmake-mode.el if it exists
(require 'cmake-mode nil t)

;; Make the buffer list better
(bind-key "C-x C-b" #'electric-buffer-list)

;; Always display line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'column-number-mode)

;; Side buttons on mouse to move between buffers
(bind-key "<mouse-9>" #'next-buffer)
(bind-key "<mouse-8>" #'previous-buffer)

;; Enable scrolling the text left with C-x <
(put 'scroll-left 'disabled nil)
;; Press a in dired to open without creating a new buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Extension for common lisp (.cl)
(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode))
;; .stumpwmrc file should open in common-lisp-mode
(add-to-list 'auto-mode-alist '("/.stumpwmrc\\'" . common-lisp-mode))

;; Frame Configuration
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Put autosaves in ~/.emacs.d/
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Stuff from customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(highlight-indent-guides-method 'bitmap)
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(highlight-indent-guides multiple-cursors meson-mode gdscript-mode magithub magit rainbow-mode clang-format+ clang-format lsp-ui lsp-mode sly flycheck gruvbox-theme which-key use-package company)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 112 :width normal :foundry "*" :family (if (= (getenv "HOSTNAME") "machine") "Terminus" "Monospace"))))))

(provide 'init)
;;; init.el ends here
