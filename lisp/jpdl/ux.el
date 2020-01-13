;;; jpdl/ux --- Summary
;;; Commentary:

;;; Code:

(require 'org)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; Small indicator for empty lines
(setq-default indicate-empty-lines t)

;; No tabs
(setq tab-width 4)
(setq indent-tabs-mode nil)

;; no scroll bars
(set-window-scroll-bars (minibuffer-window) nil nil)

;; Set frame title
(setq frame-title-format '((:eval (projectile-project-name))))

;; No start-up mesage
(setq inhibit-startup-message t)

;; Themes
;;; all the icons available
(use-package all-the-icons)

;; Gruvbox
(use-package gruvbox-theme
  :config
(load-theme 'gruvbox-dark-soft)
  (enable-theme 'gruvbox-dark-soft))

;; Doom-modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (doom-modeline-evil-emacs-state ((t (:inherit bold :foreground "dark magenta"))))
  (doom-modeline-evil-insert-state ((t (:inherit bold :foreground "deep sky blue"))))
  :config
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-vcs-icon t)
  (setq find-file-visit-truename t)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-minor-modes (featurep 'minions)))

(use-package centaur-tabs
  :demand
  :init (setq centaur-tabs-set-bar 'over)
  :config
  (centaur-tabs-mode +1)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modifiedo-marker " ● "
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-height 30
        centaur-tabs-set-icons t
        centaur-tabs-close-button " × ")
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("<C-S-tab>" . centaur-tabs-backward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-w" . kill-buffer))

;;; Powerline pls
;; (use-package powerline
;;   :init
;;   (setq powerline-default-separator 'arrow
;;         powerline-default-separator-dir (quote (left . right))
;;         powerline-height 18
;;         powerline-display-buffer-size nil
;;         powerline-display-hud nil
;;         powerline-display-mule-info nil
;;         powerline-gui-use-vcs-glyph t
;;         powerline-inactive1 '((t (:background "grey11" :foreground "#c5c8c6")))
;;         powerline-inactive2 '((t (:background "grey20" :foreground "#c5c8c6"))))
;;   :config
;;   (powerline-default-theme))

;; (use-package airline-themes
;;   :config
;;   (load-theme 'airline-gruvbox-dark t)
;;   (setq powerline-utf-8-separator-left        #xe0b0
;; 		powerline-utf-8-separator-right       #xe0b2
;; 		airline-utf-glyph-separator-left      #xe0b0
;; 		airline-utf-glyph-separator-right     #xe0b2
;; 		airline-utf-glyph-subseparator-left   #xe0b1
;; 		airline-utf-glyph-subseparator-right  #xe0b3
;; 		airline-utf-glyph-branch              #xe0a0
;; 		airline-utf-glyph-readonly            #xe0a2
;; 		airline-utf-glyph-linenumber          #xe0a1))

;; Font
(setq default-font "Fira Code")
(setq default-font-size 10)
(setq current-font-size default-font-size)
  
(setq font-change-increment 1.1)

;; Line numbers
(use-package nlinum-relative
  :config
  (global-nlinum-relative-mode)
  (nlinum-relative-setup-evil))

;; we live by the snippets
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
  (yas-global-mode 1))

;; all hail evil-mode
(setq evil-want-abbrev-expand-on-insert-exit nil) 

(use-package evil
  :config
  (evil-mode 1)
  (setq evil-emacs-state-mode nil)
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
  :bind
  ("C-s" . evil-write))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme))))

(use-package ivy
  :ensure t
  :init
  (setq ivy-mode t)
  (setq ivy-count-format "(%d/%d) ") ; display (current/total) instead of just total
  (setq ivy-format-function 'ivy-format-function-line) ; highlight the entire line
  (setq ivy-use-selectable-prompt t))

(use-package ivy-posframe
  :after ivy
  :diminish
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20)))
  ivy-posframe-parameters '((internal-border-width . 10))
  (setq ivy-posframe-width 70)
  (ivy-posframe-mode +1))


(use-package ivy-rich
  :preface
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (all-the-icons-icon-for-mode major-mode)))
  :init
  (setq ivy-rich-display-transformers-list ; max column width sum = (ivy-poframe-width - 1)
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon (:width 2))
            (ivy-rich-candidate (:width 35))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-major-mode (:width 13 :face warning)))
           :predicate
           #'(lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 35))
            (ivy-rich-counsel-function-docstring (:width 34 :face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 35))
            (ivy-rich-counsel-variable-docstring (:width 34 :face font-lock-doc-face))))
          package-install
          (:columns
           ((ivy-rich-candidate (:width 25))
            (ivy-rich-package-version (:width 12 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:width 23 :face font-lock-doc-face))))))
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel
  :init  
  (setq counsel-mode t))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

;; Zoom-in and zoom-out
(defun font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (concat default-font "-" (number-to-string current-font-size)))
  
(defun set-font-size ()
  "Set the font to `default-font' at `current-font-size'.
  Set that for the current frame, and also make it the default for
  other, future frames."
  (let ((font-code (font-code)))
    (add-to-list 'default-frame-alist (cons 'font font-code))
    (set-frame-font font-code)))

(defun reset-font-size ()
  "Change font size back to `default-font-size'."
  (interactive)
  (setq current-font-size (or default-font-size 10))
  (set-font-size))

(defun increase-font-size ()
  "Increase current font size by a factor of `font-change-increment'."
  (interactive)
  (setq current-font-size
        (ceiling (* current-font-size font-change-increment)))
  (set-font-size))

(defun decrease-font-size ()
  "Decrease current font size by a factor of `font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq current-font-size
        (max 1
             (floor (/ current-font-size font-change-increment))))
  (set-font-size))

(define-key global-map (kbd "C-)") 'reset-font-size)
(define-key global-map (kbd "C-+") 'increase-font-size)
(define-key global-map (kbd "C-=") 'increase-font-size)
(define-key global-map (kbd "C-_") 'decrease-font-size)
(define-key global-map (kbd "C--") 'decrease-font-size)

(reset-font-size)

;; Minions
(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))


(provide 'jpdl/ux)
;;; jpdl/ux.el ends here
