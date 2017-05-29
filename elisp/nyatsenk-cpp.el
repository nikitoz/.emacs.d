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
  (define-key global-map (kbd "C-M-g") 'rtags-find-references-at-point))

(provide 'nyatsenk-cpp)