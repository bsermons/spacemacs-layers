;;; packages.el --- bs-purescript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq bs-purescript-packages
    '(
      company
      purescript-mode
      (psci :location local)
      (psc-ide :location local)
      ))

;; List of packages to exclude.
(setq bs-purescript-excluded-packages '())


(when (configuration-layer/layer-usedp 'auto-completion)
  (defun bs-purescript/pre-init-psc-ide ()
    (use-package psc-ide
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (push 'company-psc-ide-backend company-backends-purescript-mode)
      (push 'company-psc-ide-frontend company-backends-purescript-mode))))


(defun bs-purescript/post-init-company ()
  (spacemacs|add-company-hook purescript-mode))


(defun bs-purescript/init-psc-ide ()
  (use-package psc-ide
    :defer t
    :init (add-hook 'purescript-mode-hook
                    (lambda ()
                      (company-mode)
                      (psc-ide-mode)))))


(defun bs-purescript/init-purescript-mode ()
  (use-package purescript-mode
    :defer t
    :init
    (progn
      (require 'psci)
      (require 'psc-ide)
      (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
      (evil-leader/set-key-for-mode 'purescript-mode
        "mi="  'purescript-mode-format-imports
        "mi`"  'purescript-navigate-imports-return
        "mia"  'purescript-align-imports
        "min"  'purescript-navigate-imports))))


(defun bs-purescript/init-psci ()
  (use-package psci
    :defer t
    :init
    (progn
      (add-hook 'purescript-mode-hook 'inferior-psci-mode)
      (evil-leader/set-key-for-mode 'purescript-mode
        "msb" 'psci/load-current-file!
        "msi" 'psci
        "msm" 'psci/load-module!
        "msp" 'psci/load-project-modules!))))
