;;; package -- Summary:
;;; I'm too lazy for this shit.

;;; Commentary:

;;; Code:

;; Load custom-file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)

;; Configure package.el to include MELPA.
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Makes sure that all packages are present
(setq use-package-always-ensure t)

;; Ensure to always compile
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)
;; (org-babel-load-file "~/.emacs.d/configuration.org")

;; Booting
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'jpdl/user)
(require 'jpdl/utils)
(require 'jpdl/ux)
(require 'jpdl/managers)
(require 'jpdl/org)
(require 'jpdl/prog)

(provide 'init.el)
;;; init.el ends here
