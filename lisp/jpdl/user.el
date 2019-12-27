;;; jpdl/user --- Summary

;;; Commentary:

;;; Code:

(require 'use-package)

(setq home (concat "/home/" (eval user-login-name)))

(use-package keychain-environment
  :init (keychain-refresh-environment))

(provide 'jpdl/user)
;;; jpdl/user.el ends here
