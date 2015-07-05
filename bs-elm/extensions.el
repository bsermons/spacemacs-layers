;;; extensions.el --- bs-elm Layer extensions File for Spacemacs
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

(setq bs-elm-pre-extensions
      '(
        ;; pre extension bs-elms go here
        ))

(setq bs-elm-post-extensions
      '(
        elm-mode
        flycheck-elm
        ))

(defun bs-elm/init-elm-mode ()
  "Initialize elm-mode"
  (use-package elm-mode
    :load-path "~/.emacs.d/private/bs-elm/extensions/elm-mode")
  )

(defun bs-elm/init-flycheck-elm ()
  "Initialize flycheck-elm"
  (use-package flycheck-elm
    :load-path "~/.emacs.d/private/bs-elm/extensions/flycheck-elm"))

;; For each extension, define a function bs-elm/init-<extension-bs-elm>
;;
;; (defun bs-elm/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
