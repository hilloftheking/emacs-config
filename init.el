;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun install-packages-if-required (names)
  (let ((already-refreshed nil))
    (while names
      (let ((name (car names)))
	(unless (package-installed-p name)
	  (unless already-refreshed
	    (package-refresh-contents)
	    (setq already-refreshed t))
	  (package-install name)))
      (setq names (cdr names)))))

(install-packages-if-required '(use-package which-key gruvbox-theme))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; Frame Configuration
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Stuff from customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(package-selected-packages
   '(gruvbox-theme which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "ADBO" :family "Source Code Pro")))))
