(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
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
  "Copies 'break filename:linenumber' to buffer"
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
(semantic-mode 0)
(use-package lua-mode)
(use-package color-theme-sanityinc-tomorrow)
(use-package solarized-theme)
(use-package nord-theme)
;------------------------------------------------------------------------------
; solarized
;------------------------------------------------------------------------------
(if (display-graphic-p)
    (progn
;      (load-theme 'solarized-light t)))
      (load-theme 'sanityinc-tomorrow-eighties t)))

(use-package magit)

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching nil)
  (define-key global-map (kbd "<f7>") 'projectile-compile-project)
  (define-key global-map (kbd "<f5>") 'projectile-run-project)
  (define-key global-map (kbd "<f6>") 'projectoile-test-project)
  (define-key global-map (kbd "C-M-o") 'projectile-find-other-file))
;------------------------------------------------------------------------------
; Helm
;------------------------------------------------------------------------------
(use-package helm)
(use-package helm-ag)
(custom-set-variables
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point 'word)
 '(helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'")))
(add-hook 'c-mode-common-hook 'superword-mode)

(use-package helm-projectile
  :config
  (helm-projectile-on)
  (define-key projectile-mode-map (kbd "C-c C-g") 'helm-projectile-ag)
  (define-key projectile-mode-map (kbd "C-c C-f") 'helm-projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c C-o") 'helm-projectile-find-other-file))

(use-package rainbow-delimiters
  :config
  (add-hook 'c-mode-hook 'rainbow-delimiters-mode)
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

;;(use-package ido-completing-read+
;;  :config
;;  (ido-ubiquitous-mode 1))

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
  (setq company-idle-delay 0.0))

(use-package rust-mode)
(use-package racer)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(use-package ws-butler
  :config
  (add-hook 'c-mode-common-hook 'ws-butler-mode))

(setq custom-file "~/custom.el")
(setq-default shell-file-name "/usr/bin/zsh")
(load custom-file)

(use-package realgud)
(use-package lsp-mode
  :commands lsp
  :init)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp
  :config
  (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
  (setq cquery-extra-init-params '(:completion (:detailedLabel t)))
  (push 'company-lsp company-backends))

(use-package nyatsenk-cpp :ensure f)
(use-package nyatsenk-org :ensure f)
(use-package nyatsenk-py  :ensure f)
(use-package nyatsenk-scala :ensure f)
(server-start)
;(use-package nyatsenk-last :ensure f)
