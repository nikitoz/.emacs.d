(use-package cc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-hook 'c-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
  (global-set-key (kbd "M-o") 'ff-find-other-file)
  (add-to-list 'c-mode-common-hook (lambda () (setq c-syntactic-indentation nil))))

(setq compilation-scroll-output 'first-error)

(use-package clang-format
  :config
  (global-set-key [C-f9] 'clang-format-region))

(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".h"))
    ("\\.h\\'" (".cpp"))
    ("\\.cxx\\'" (".hxx" ".ixx"))
    ("\\.ixx\\'" (".cxx" ".hxx"))
    ("\\.hxx\\'" (".ixx" ".cxx"))
    ))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

(use-package rtags
  :config
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (rtags-enable-standard-keybindings)
  (setq rtags-display-result-backend 'helm)
  (define-key global-map (kbd "C-M-g") 'rtags-find-references-at-point)
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  (define-key c-mode-base-map (kbd "C-q") (function company-complete)))

(use-package  company-rtags
  :config
  (push 'company-rtags company-backends))

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t))

(use-package ycmd
  :config
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (set-variable 'ycmd-server-command '("python" "/home/nyatsenk/hub/ycmd/ycmd"))
  (set-variable 'ycmd-extra-conf-whitelist '("~/*"))
  (define-key ycmd-mode-map ycmd-keymap-prefix nil)
  (setq ycmd-keymap-prefix (kbd "C-c y"))
  (define-key ycmd-mode-map ycmd-keymap-prefix ycmd-command-map))

(use-package company-ycmd
  :config
  (company-ycmd-setup)
  (define-key global-map (kbd "M-,") 'pop-tag-mark)
  (define-key global-map (kbd "M-.") 'ycmd-goto)
  (define-key global-map (kbd "C-M-o") 'projectile-find-other-file))

(provide 'nyatsenk-cpp)
