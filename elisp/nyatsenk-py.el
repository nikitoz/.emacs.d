(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))
; sudo -H pip install setuptools
(use-package company-anaconda)
(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
(provide 'nyatsenk-py)
