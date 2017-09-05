;; sudo apt-get install python-ropemacs
;; sudo pip install pymacs
(require 'pymacs) ;; this can't be loaded by use-package because it is not in repos
(pymacs-load "ropemacs" "rope-")

(provide 'nyatsenk-py)
