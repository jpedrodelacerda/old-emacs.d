;;; jpdl/managers --- Summary:

;;; Commentary:

;;; Code:

;; Helm
(use-package helm
  :config
  (helm-autoresize-mode 1)
  (setq helm-autoresize-min-height 25)
  (setq helm-autoresize-max-height 35)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  :bind (("C-x C-b" . helm-mini)
	     ("C-x C-f" . helm-find-files)
	     ("C-x C-d" . helm-projectile-find-file)))

;; Company
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-transformers '(company-sort-by-occurrence))
  :bind
  ("C-<tab>" . company-complete-common-or-cycle)
  ("C-'" . company-complete))
(global-company-mode)

;;; Fuzzing
(use-package company-flx
  :after
  (company)
  :config
  (company-flx-mode +1))

;; LSP completes me
(use-package lsp-mode
  :commands (lsp)
  :hook (
		 (go-mode . lsp)
		 (python-mode . lsp))
  :config
  (use-package company-lsp
	:config
	(add-to-list 'company-backends 'company-lsp)))

(defvar lsp-language-id-configuration
  '((go-mode . "go")
	(anaconda-mode . "python")))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; dumbjumps
(use-package dumb-jump
  :bind (
		 ("M-." . dumb-jump-go)
		 ("M-g o" . dumb-jump-go-other-window)
		 ("M-g j" . dumb-jump-go)
		 ("M-g i" . dumb-jump-go-prompt)
		 ("M-g x" . dumb-jump-go-prefer-external)
		 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (define-key evil-normal-state-map (kbd "M-.") 'dumb-jump-go)
  (setq dumb-jump-selector 'ivy))

;; Checking for errors
(use-package flycheck
  :config
  (global-flycheck-mode))

;; Magic handler for git
(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (use-package evil-magit)
  (use-package with-editor)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  
  ;; Start commit message in insert state.
  (add-hook 'with-editor-mode-hook 'evil-insert-state))
(use-package evil-magit)

;; Handle projects
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/"))
  (setq projectile-completion-system 'helm)
  (which-key-add-key-based-replacements "C-c p 4" "other-window"
    "C-c p 5" "other-frame"
    "C-c p s" "search"
    "C-c p x" "execute")
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :ensure t
  :init (helm-projectile-on))

;; Browse directories and projects
(use-package treemacs
  :config
  (use-package treemacs-evil)
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  :bind ("M-v" . treemacs ))

;; Undo function
(use-package undo-tree)

;; Nerd commentaries
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; tokens beutifier and completer
(use-package smartparens
  :ensure t
  :init
  :bind ("C-SPC" . sp-forward-sexp)
  :config
  (sp-pair "{" nil :post-handlers '((jpdl/create-newline-and-enter-sexp "RET")))
  (sp-pair "[" nil :post-handlers '((jpdl/create-newline-and-enter-sexp "RET")))
  (sp-pair "(" nil :post-handlers '((jpdl/create-newline-and-enter-sexp "RET")))
  (require 'smartparens-config)
  (smartparens-global-mode 1))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'jpdl/managers)
;; jpdl/managers.el ends here
