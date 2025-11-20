(context-menu-mode 1)
(tool-bar-mode -1)

(setq make-backup-files nil)
(setq gdb-many-windows t)
(setq dired-listing-switches (concat dired-listing-switches "h"))
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(column-number-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(keymap-global-set "C-x b" 'electric-buffer-list)
(keymap-global-set "C-x C-b" 'switch-to-buffer)
(keymap-global-set "C-c q" 'calculator)

;; dired opens a new window with each click by default which is dumb
(with-eval-after-load "dired"
  (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file))

(setq mouse-wheel-progressive-speed nil)
(setcar mouse-wheel-scroll-amount 5)

(load "~/.emacs.d/exotica-theme.el")
(load-theme 'exotica t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package eglot
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider)))
(use-package vertico
  :config
  (vertico-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring
(use-package magit)
(use-package corfu
  :custom
  (corfu-auto t)
  :config
  (global-corfu-mode)
  ;; Don't use enter key for completions
  (keymap-unset corfu-map "RET"))
(use-package meow)
(use-package which-key
  :config
  (which-key-mode 1))
(use-package glsl-mode)
(use-package lua-mode)
(use-package clang-format-lite
  :hook
  ((c-mode . clang-format-lite-save-hook) (c++-mode . clang-format-lite-save-hook)))
(use-package editorconfig
  :config
  (editorconfig-mode 1))
(use-package eat
  :hook ((eshell-load . eat-eshell-mode) (eshell-load . eat-eshell-visual-command-mode)))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-use-clipboard t)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
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
   '("<escape>" . ignore)))

(meow-setup)
(meow-global-mode 1)

