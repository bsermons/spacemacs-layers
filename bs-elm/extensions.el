;;; extensions.el --- bs-elm Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Brian Sermons & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq bs-elm-pre-extensions
      '(
        ;; pre extension bs-elms go here
        ))

(setq bs-elm-post-extensions
      '(
        elm-mode
        flycheck-elm
        company-elm
        ))

(defun bs-elm/init-elm-mode ()
  "Initialize elm-mode"
  (use-package elm-mode
    ;:load-path "~/.emacs.d/private/bs-elm/extensions/elm-mode"
    :defer t
    :config
    (evil-leader/set-key-for-mode 'elm-mode
      ;; Compile
      "mcc" 'elm-compile-buffer
      "mcC" 'elm-compile-main

      ;; REPL
      "msi" 'elm-load-repl
      "msr" 'push-elm-repl
      "msp" 'push-decl-elm-repl

      ;; Misc
      "mn" 'elm-preview-buffer
      "mm" 'elm-preview-main)
    :init
    (progn
      (require 'elm-mode)
      (require 'flycheck-elm)
      (require 'company-elm)))) 

(defun bs-elm/init-flycheck-elm ()
  "Initialize flycheck-elm"
  (use-package flycheck-elm
    :defer t
    :init (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun bs-elm/post-init-company ()
    (spacemacs|add-company-hook elm-mode))

  ;; Add the backend to the major-mode specific backend list
  (defun bs-elm/init-company-elm ()
    (use-package company-elm
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (progn
        (push 'company-elm-backend company-backends-elm-mode)))))

;(push 'company-elm company-backends-elm-mode))))
