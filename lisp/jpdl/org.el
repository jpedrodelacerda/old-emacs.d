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

(use-package org-agenda
  :ensure nil
  :after org
  :bind ("C-c c" . org-capture)
  :config
  (setq org-agenda-files (quote ("~/MEGAsync/agenda/pessoal.org"
								 "~/MEGAsync/agenda/gris.org"
								 "~/MEGAsync/agenda/eci.org"
								 "~/MEGAsync/agenda/capgov.org"
								 )))
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
  (setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

  (setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  (setq org-capture-templates
      (quote (("t" "todo" entry (file "~/MEGAsync/agenda/refile.org")
               "* TODO %?\n%U\n")
              ("r" "respond" entry (file "~/MEGAsync/agenda/refile.org")
               "* NEXT Responder %:from sobre %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
              ("n" "note" entry (file "~/MEGAsync/agenda/refile.org")
               "* %? :NOTE:\n%U\n%a\n")
              ("j" "Journal" entry (file+datetree "~/MEGAsync/agenda/journal.org")
               "* %?\n%U\n")
              ("w" "org-protocol" entry (file "~/MEGAsync/agenda/refile.org")
               "* TODO Revisar %c\n%U\n" :immediate-finish t)
              ("m" "Reunião" entry (file "~/MEGAsync/agenda/refile.org")
               "* MEETING %? :MEETING:\n%U")
              ("p" "Ligação" entry (file "~/MEGAsync/agenda/refile.org")
               "* PHONE %? :PHONE:\n%U")
              ("h" "Habit" entry (file "~/MEGAsync/agenda/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
  )

(provide 'jpdl/org)
;;; jpdl/org.el ends here
