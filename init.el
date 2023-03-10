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
  :config
  (which-key-mode))
(use-package company
  :config
  (global-company-mode))
(use-package lsp-mode
  :config
  (add-hook 'c-mode-hook 'lsp-mode)
  (add-hook 'cpp-mode-hook 'lsp-mode)
  (add-hook 'objc-mode-hook 'lsp-mode))
(use-package magit)
(use-package magithub)
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"))
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package clang-format+
  :config
  (add-hook 'c-mode-hook 'clang-format+-mode)
  (add-hook 'cpp-mode-hook 'clang-format+-mode))
(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))
(use-package gruvbox-theme)

;; Make the buffer list better
(bind-key "C-x C-b" #'electric-buffer-list)

;; Always display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'column-number-mode)

;; Side buttons on mouse to move between buffers
(bind-key "<mouse-9>" 'next-buffer)
(bind-key "<mouse-8>" 'previous-buffer)

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
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(magithub magit rainbow-mode clang-format+ clang-format lsp-ui lsp-mode sly flycheck gruvbox-theme which-key use-package company)))

(custom-set-faces
 ;; Terminus on desktop, default for monospace on laptop
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 112 :width normal :foundry "*"
			 :family (if (= (getenv "HOSTNAME") "machine") "Terminus" "Monospace"))))))
