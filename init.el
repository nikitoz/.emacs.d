(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'use-package)
(setq use-package-always-ensure t)
(global-auto-revert-mode) ; enable auto-reload
;------------------------------------------------------------------------------
; Source code pro is adobe fonts
;------------------------------------------------------------------------------
(set-default-font "Source Code Pro Medium")
(set-face-attribute 'default nil :height 110)
;------------------------------------------------------------------------------
; CUA mode sets ctrl-c/v and that kind of stuff
;------------------------------------------------------------------------------
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(set-frame-width (selected-frame) 120)
;------------------------------------------------------------------------------
; TAB
;------------------------------------------------------------------------------
(tool-bar-mode -1)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-indent 4)
;------------------------------------------------------------------------------
; mouse scroll
;------------------------------------------------------------------------------
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
;------------------------------------------------------------------------------
; setting toolbar
;------------------------------------------------------------------------------
(tool-bar-mode -1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq c-backspace-function 'backward-delete-char)
(setq w32-pipe-read-delay 0)
;------------------------------------------------------------------------------
; Emacs droppings will be copied to special folder
;------------------------------------------------------------------------------
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))
;------------------------------------------------------------------------------
; Some utility functions
;------------------------------------------------------------------------------
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f8] 'show-file-name)

(defun copy-for-gdb ()
  "Copies filename:linenumber to buffer"
  (interactive)
  (kill-new (concat "break " (buffer-name) ":" (number-to-string (line-number-at-pos)))))
(global-set-key [C-f1] 'copy-for-gdb)
;------------------------------------------------------------------------------
; Dedicated windows
;------------------------------------------------------------------------------
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message 
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(global-set-key [pause] 'toggle-window-dedicated)

(add-hook 'emacs-lisp-mode-hook
    (lambda ()
    (setq indent-tabs-mode nil)
    (semantic-mode)))
;------------------------------------------------------------------------------
; solarized
;------------------------------------------------------------------------------
(load-theme 'sanityinc-solarized-light)

(use-package projectile
  :config
  (projectile-global-mode)
  (define-key global-map (kbd "<f7>") 'projectile-compile-project)
  (define-key global-map (kbd "<f5>") 'projectile-run-project)
  (define-key global-map (kbd "<f6>") 'projectile-test-project))
;------------------------------------------------------------------------------
; Helm
;------------------------------------------------------------------------------
(use-package helm)
(use-package helm-ag)
(use-package helm-projectile
  :config
  (helm-projectile-on)
  (define-key projectile-mode-map (kbd "C-c C-g") 'helm-projectile-ag)
  (define-key projectile-mode-map (kbd "C-c C-f") 'helm-projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c C-o") 'helm-projectile-find-other-file))

(use-package rainbow-delimiters
  :config
  (add-hook 'c-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
;------------------------------------------------------------------------------
; Tabs for documents - extremely handy
;------------------------------------------------------------------------------
(use-package tabbar
  :config
  (tabbar-mode t)
  (set-face-attribute 'tabbar-default nil :height 100)
  (set-face-attribute 'tabbar-default nil :background "#073642")
  (set-face-attribute 'tabbar-default nil :foreground "#93a1a1")
  (set-face-attribute 'tabbar-selected nil :foreground "#859900"))
;------------------------------------------------------------------------------
; IDO mode - fuzzy search for files
;------------------------------------------------------------------------------
(use-package ido)
(use-package flx-ido
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

;------------------------------------------------------------------------------
; Set highlight
;------------------------------------------------------------------------------
(use-package highlight-symbol
  :config
  (global-set-key [(f4)] 'highlight-symbol-at-point)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))
;------------------------------------------------------------------------------
; Auto highlight mode
;------------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t)
;;  (set-face-attribute 'ahs-plugin-default-face nil :background "#657b83")
;;(set-face-attribute 'ahs-plugin-default-face nil :foreground "GhostWhite")
  (set-face-attribute 'ahs-face nil :background "#657b83")
  (set-face-attribute 'ahs-face nil :foreground "#eee8d5"))
;-----------------------------------------------------------------------------
; Speedbar stuff
;-----------------------------------------------------------------------------
(setq speedbar-use-images nil)
(add-hook
 'speedbar-load-hook
 (lambda()
   (progn
	 (set-face-attribute 'speedbar-file-face nil :foreground "#93a1a1")
	 (set-face-attribute 'speedbar-selected-face nil :foreground "#859900"))))
;-----------------------------------------------------------------------------
; Company
;-----------------------------------------------------------------------------
(use-package color)
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-global-modes '(not gud-mode))
  (global-company-mode 1)
  (setq company-idle-delay 0.0)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(use-package ws-butler
  :config
  (add-hook 'c-mode-common-hook 'ws-butler-mode))

(setq custom-file "~/custom.el")
(load custom-file)

(use-package realgud)

(use-package nyatsenk-cpp :ensure f)
(use-package nyatsenk-org :ensure f)
(use-package nyatsenk-py  :ensure f)
