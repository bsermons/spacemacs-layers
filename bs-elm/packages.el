;;; packages.el --- bs-elm Layer packages File for Spacemacs
;;; License: GPLv3

;;; Commentary:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.

;;; Code:

(setq bs-elm-packages
    '(
      flycheck
      ))

;; List of packages to exclude.
(setq bs-elm-excluded-packages '())

(defun bs-elm/init-flycheck ()
  "Initialize my package"
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))
