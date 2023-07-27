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
(setq use-package-compute-statistics t)

(use-package delight)
(use-package which-key
  :config
  (which-key-mode)
  :delight)
(use-package company
  :config
  (global-company-mode)
  :delight)
(use-package bind-key)
(use-package ivy
  :config
  (ivy-mode)
  :delight)
(use-package swiper
  :bind
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-backward)))
(use-package treemacs
  :bind
  ("C-c t" . treemacs))
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook
  ((c-mode-common . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :bind
  (:map lsp-command-map
        ("g a" . lsp-ivy-workspace-symbol))
  :defer t)
(use-package lsp-ui
  :config
  (progn
    (setq lsp-ui-doc-show-with-mouse nil) ;; Fixes mouse movement cancelling chords, disables mouse hover tooltips
    (setq lsp-ui-doc-position 'at-point))
  :commands lsp
  :bind
  (:map lsp-command-map
        ("C-l" . lsp-ui-doc-glance)))
(use-package lsp-ivy)
(use-package magit
  :commands
  magit)
(use-package flycheck
  :config
  (global-flycheck-mode))
(use-package clang-format
  :init
  (add-hook
   'c-mode-common-hook
   (lambda ()
     (setq clang-format-fallback-style "llvm"))))
(use-package format-all
  :hook
  ((prog-mode . format-all-mode)
   (format-all-mode . format-all-ensure-formatter))
  :delight)
(use-package meson-mode)
(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  :delight)
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
(use-package dtrt-indent
  :config
  (dtrt-indent-global-mode)
  :delight)
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'bitmap)
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :delight)
(use-package cmake-mode)

;; Make the buffer list better
(bind-key "C-x C-b" #'electric-buffer-list)

;; Always display line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'column-number-mode)

;; Side buttons on mouse to move between buffers
(bind-key "<mouse-9>" #'next-buffer)
(bind-key "<mouse-8>" #'previous-buffer)

;; Stops Windows from making error sound
(setq visible-bell 1)

;; Enable scrolling the text left with C-x <
(put 'scroll-left 'disabled nil)
;; Press a in dired to open without creating a new buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Frame Configuration
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Hide some stuff from the modeline
(delight 'abbrev-mode nil 'abbrev)
(delight 'auto-revert-mode nil 'autorevert)
(delight 'eldoc-mode nil 'eldoc)

;; Put autosaves in ~/.emacs.d/
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Use Terminus font if it exists, otherwise use Monospace default
(defun font-exists-p (font) "Check if FONT exists." (if (null (x-list-fonts font)) nil t))
(set-face-attribute 'default nil :height 112 :family (if (font-exists-p "Terminus") "Terminus" "Monospace"))

;; Stuff from customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(lsp-ivy swiper ivy delight cmake-mode dtrt-indent treemacs format-all highlight-indent-guides multiple-cursors meson-mode gdscript-mode magit rainbow-mode clang-format+ clang-format lsp-ui lsp-mode flycheck gruvbox-theme which-key use-package company))
 '(warning-suppress-types '((lsp-mode))))

(provide 'init)
;;; init.el ends here
