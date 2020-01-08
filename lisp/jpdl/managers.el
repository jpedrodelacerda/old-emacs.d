;;; jpdl/managers --- Summary:

;;; Commentary:

;;; Code:

(global-auto-revert-mode t)

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
  ;; ("C-<tab>" . company-complete-common-or-cycle)
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
  :bind (:map lsp-mode-map
		 (("C-c C-f" . lsp-format-buffer)))
  :hook ((elixir-mode . lsp)
		 (go-mode . lsp)
		 (python-mode . lsp))
  :custom
  (lsp-prefer-flymake nil)
  :config
  (use-package company-lsp
	:config
	(push 'company-lsp company-backends)))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :diminish
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  :config
  (setq lsp-ui-doc-use-webkit t)
  )

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
  :diminish smartparens-mode
  :bind ("C-SPC" . sp-forward-sexp)
  :commands (smartparens-mode show-smartparens-mode)
  :init
  (smartparens-global-mode 1)
  (smartparens-strict-mode 1)
  (show-smartparens-global-mode t)
  (setq smartparens-global-mode t)
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'jpdl/managers)
;; jpdl/managers.el ends here
