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
(use-package meow ;; This is kind of big. Maybe I will move it to a diff file
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("n" . "H-j")
   '("p" . "H-k")
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC n/p will run the original command in MOTION state.
   '("n" . "H-n")
   '("p" . "H-p")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  (meow-global-mode))
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
  (setq lsp-keymap-prefix "\\")
  :hook
  ((c-mode-common . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :bind
  (:map lsp-command-map
        ("g a" . lsp-ivy-workspace-symbol))
  :defer t)
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-show-with-mouse nil ;; Fixes mouse movement cancelling chords, disables mouse hover tooltips
        lsp-ui-doc-position 'at-point)
  :commands lsp
  :bind
  (:map lsp-command-map
        ("l" . lsp-ui-doc-glance)))
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
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))
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
(use-package ace-window
  :config
  (meow-normal-define-key
   '(":" . ace-window)))

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

;; Use Terminus font
(add-to-list 'default-frame-alist '(font . "Terminus-10"))

;; Stuff from customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(meow lsp-ivy swiper ivy delight cmake-mode dtrt-indent treemacs format-all highlight-indent-guides multiple-cursors meson-mode gdscript-mode magit rainbow-mode clang-format+ clang-format lsp-ui lsp-mode flycheck gruvbox-theme which-key use-package company))
 '(warning-suppress-types '((lsp-mode))))

(provide 'init)
;;; init.el ends here
