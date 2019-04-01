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
    ("\\.hh\\'" (".cc"))
    ("\\.cxx\\'" (".hxx" ".ixx"))
    ("\\.ixx\\'" (".cxx" ".hxx"))
    ("\\.hxx\\'" (".ixx" ".cxx"))))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t))

(use-package cquery
  :config
  (setq cquery-executable "/home/nyatsenk/hub/cquery/build/release/bin/cquery"))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(provide 'nyatsenk-cpp)
