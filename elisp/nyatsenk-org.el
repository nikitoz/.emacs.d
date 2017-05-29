(add-hook 'org-mode-hook #'toggle-word-wrap)
(setq org-startup-indented t) 
(add-hook 'org-mode-hook #'(lambda ()
                             ;; make the lines in the buffer wrap around the edges of the screen.
                             ;; to press C-c q  or fill-paragraph ever again!
                             (visual-line-mode)
                             (org-indent-mode)))

(provide 'nyatsenk-org)
