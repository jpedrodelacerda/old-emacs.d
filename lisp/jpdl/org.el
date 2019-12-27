;;; jpdl/org --- Summary:

;;; Commentary:

;;; Code:
(use-package org)

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (setq org-ellipsis "...")
(setq org-ellipsis " ⤵")

(setq org-src-tab-acts-natively t)

(use-package ox-reveal
  :pin melpa)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-support-shift-select 'always)


;; Trying emacs-jupyter
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                    (:session . "py")
                                                    (:kernel . "python3")))

(provide 'jpdl/org)
;;; jpdl/org.el ends here
