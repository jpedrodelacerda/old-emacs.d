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

;; no scroll bars
(set-window-scroll-bars (minibuffer-window) nil nil)

;; Set frame title
(setq frame-title-format '((:eval (projectile-project-name))))

;; No start-up mesage
(setq inhibit-startup-message t)

;; Themes
;;; all the icons available
(use-package all-the-icons)

;;; Gruvbox
(use-package gruvbox-theme 
  :config
(load-theme 'gruvbox-dark-soft)
  (enable-theme 'gruvbox-dark-soft))

;;; Powerline pls
(use-package powerline
  :init
  (setq powerline-default-separator 'arrow
        powerline-default-separator-dir (quote (left . right))
        powerline-height 18
        powerline-display-buffer-size nil
        powerline-display-hud nil
        powerline-display-mule-info nil
        powerline-gui-use-vcs-glyph t
        powerline-inactive1 '((t (:background "grey11" :foreground "#c5c8c6")))
        powerline-inactive2 '((t (:background "grey20" :foreground "#c5c8c6"))))
  :config
  (powerline-default-theme))

(use-package airline-themes
  :config
  (load-theme 'airline-gruvbox-dark t)
  (setq powerline-utf-8-separator-left        #xe0b0
		powerline-utf-8-separator-right       #xe0b2
		airline-utf-glyph-separator-left      #xe0b0
		airline-utf-glyph-separator-right     #xe0b2
		airline-utf-glyph-subseparator-left   #xe0b1
		airline-utf-glyph-subseparator-right  #xe0b3
		airline-utf-glyph-branch              #xe0a0
		airline-utf-glyph-readonly            #xe0a2
		airline-utf-glyph-linenumber          #xe0a1))

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


(provide 'jpdl/ux)
;;; jpdl/ux.el ends here
