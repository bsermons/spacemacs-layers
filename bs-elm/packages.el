;;; packages.el --- bs-elm Layer packages File for Spacemacs
;;; License: GPLv3

;;; Commentary:

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.

;;; Code:

(setq bs-elm-packages
    '(
      flycheck
      f
      s
      let-alist
      ))

;; List of packages to exclude.
(setq bs-elm-excluded-packages '())

(defun bs-elm/init-flycheck ()
  "Initialize flycheck"
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

(defun bs-elm/init-f ())

(defun bs-elm/init-s ())

(defun bs-elm-init-let-alist ())
