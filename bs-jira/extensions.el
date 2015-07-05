;;; extensions.el --- personal Layer extensions File for Spacemacs
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

(defvar personal-pre-extensions
  '(
    ;elm-simple-indent
    )
  "List of all extensions to load before the packages.")

(defvar personal-post-extensions
  '(
    ;; post extension personals go here
    ;;elm-mode
    )
  "List of all extensions to load after the packages.")

(defun personal/init-elm-mode ()
  "Initialize my extension"

  )
;; For each extension, define a function personal/init-<extension-personal>
;;
;; (defun personal/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
