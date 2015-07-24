;;; company-elm.el --- Elm support for comany-mode
;;; Copyright 2015 - Brian Sermons

;;; Package require: ((elm-util . t))

;;; Commentary:
;; To use add this to your init.el
;; (eval-after-load 'company
;;    (add-to-list 'company-backends 'company-elm))
;;; Code:
(require 'company)
(require 'elm-util)
(require 'elm-interactive)
(require 'rx)
(require 'f)
(require 's)

(defconst company-elm--cache-dir ".cache"
  "Directory to store cached documentation files.")

(defconst company-elm-exact-dependency-file "exact-dependencies.json")

;;"(?:^|\\n)import\\s([\\w\\.]+)(?:\\sas\\s(\\w+))?(?:\\sexposing\\s*\\(((?:\\s*(?:\\w+|\\(.+\\))\\s*,)*)\\s*((?:\\.\\.|\\w+|\\(.+\\)))\\s*\\))?"
;;import regex
;;import <..>
;;"\\(?:^\\|\n\\)import\\s-\\([[:word:]\\.]+\\)"
;;as <..>
;;"\\(?:\\s-as\\s-\\(\\w+\\)\\)?"
;;exposing
;;"\\(\\s-*exposing\\s-*"
;;(..)
;; "\(\\(\\(?:\\s-*\\(?:\\w+\\|\(.+\)\\)\\s-*,\\)*\\)\\s-*\\(\\(?:\.\.\\|\\w+\\|\(.+\)\\)\\)\\s-*\)"

;;TODO: this regex doesn't seem to work always
;; (defconst company-elm--import-regex
;;   "\\(?:^\\|\n\\)import\\s-\\([[:word:]\\.]+\\)\\(?:\\s-as\\s-\\(\\w+\\)\\)?\\(\\s-exposing\\s-*\(\\(\\(?:\\s-*\\(?:\\w+\\|\(.+\)\\)\\s-*,\\)*\\)\\s-*\\(\\(?:\.\.\\|\\w+\\|\(.+\)\\)\\)\\s-*\)\\)?")

(defconst company-elm--default-import-list
    ;; '(
    ;;   '(name . "Basics") '(as . "") '(exposing . (list))
    ;;   '(name . "List") '(as . "") '(exposing . '("Just" "Nothing"))
    ;;   '(name . "Result") '(as . "") '(exposing . '("Ok" "Err"))
    ;;   '(name . "Signal") '(as . "") '(exposing . '("Signal"))
    ;;   )
  (with-temp-buffer
    (goto-char 0)
    (insert "
import Foo as Bar exposing (doo,kie)
import Basics exposing (..)
import List exposing (List, (::))
import Maybe exposing ( Maybe( Just, Nothing ) )
import Result exposing ( Result( Ok, Err ) )
import Signal exposing ( Signal )
")
    (company-elm--parse-imports-in-buffer)))


(defconst company-elm--import-regex
  (rx line-start "import" (0+ space) (group (1+ (in word ".")))
      (opt (1+ space) "as" (1+ space) (group (1+ word)))
      (opt (1+ space) "exposing" (1+ space) "(" (0+ space)
           (or
            (and (group (1+ word)) (0+ (0+ space) "," (0+ space) (group (1+ word))))
            (and (1+ word) (0+ space) "(" (0+ space) (group (1+ word)) (0+ (0+ space) "," (0+ space) (group (1+ word))) ")"))
           (0+ space) ")")))


(defun company-elm--get-import-from-match-data (&optional string)
  (let* ((data (match-data))
         (len (length data))
         (idx 3)
         result exposed)
    (push `(name . ,(match-string 1 string)) result)
    (push `(as . ,(match-string 2 string)) result)
    (while (< idx len)
      (let* ((smatch (match-string idx string))
             (exp (if smatch (s-trim (s-chop-suffix "," smatch)))))
        (when (s-present? exp)
          (push exp exposed)))
      (set 'idx (1+ idx)))
    (push `(exposing . ,exposed) result)
    result))


(defun company-elm--parse-imports-in-buffer (&optional buffer)
  "Parse the list of imports for the current elm BUFFER."
  (let (matches)
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp company-elm--import-regex nil t 1)
              (push (company-elm--get-import-from-match-data) matches))))))
      matches))


(defun company-elm--read-exact-dependency-file (&optional err)
  "Read the current exact dependencies of the project.

ERR is a error callback handler if the dependency file can't be found."
  (let ((pkg-dir (elm-find-package-dir))
        (json-key-type 'string))
    (if (not (f-exists? pkg-dir))
      (if (functionp err) (funcall err))
      (json-read-file (f-join pkg-dir company-elm-exact-dependency-file)))))

(defun company-elm--parse-package-documentation (package version data)
  (json-read-from-string data))

(defun company-elm--get-http-response ()
  (with-current-buffer (current-buffer)
    (goto-char 0)
    (search-forward "\n\n" nil t)
    (buffer-substring (point) (point-max))))

;;  url format http://package.elm-lang.org/packages/USER/PACKAGE/X.Y.Z/documentation.json
(defun company-elm--fetch-package-documentation (after-load package version &optional cache-dir)
  "Read the package documentation for the given package version."
  (cl-labels ((callback (status package version cache-dir)
                        (let ((contents (company-elm--get-http-response)))
                          (when cache-dir
                            (let ((adir (f-join cache-dir package version)))
                              (when (not (f-exists? adir))
                                (make-directory adir t))
                              (f-write contents 'utf-8 (f-join adir "documentation.json"))))
                          (after-load (company-elm--parse-package-documentation package version contents)))))

    (let ((url (elm-package--build-uri "packages" package version "documentation.json")))
      (if company-elm--retrieve-synchronous
          (progn
            (with-current-buffer (url-retrieve-synchronously url)
              (callback () package version cache-dir)))
        (url-retrieve url callback
                      (list package version cache-dir))))))

(defun company-elm--load-package-documentation (cache-dir package version)
  "Load the documentation either from cache or download from the package server."
  (cl-labels ((after-load (document)
                          (setq company-elm-documentation
                                (append document company-elm-documentation))))
    (let ((docfile (f-join cache-dir package version "documentation.json"))
          documents)
      (if (f-exists? docfile)
          (after-load (company-elm--parse-package-documentation package version
                                                                (f-read docfile)))
        (company-elm--fetch-package-documentation #'after-load package version cache-dir)))))

(defun company-elm--get-cache-dir ()
  (if (f-absolute? company-elm--cache-dir)
      company-elm--cache-dir
    (f-join (elm-find-package-dir) company-elm--cache-dir)))

(defun company-elm--load-dependencies (cache-dir dependencies)
  (dolist (dependency dependencies)
    (company-elm--load-package-documentation cache-dir
                                             (car dependency)
                                             (cdr dependency))))

(defun company-elm--init ()
  (interactive)

  (set (make-local-variable 'company-elm-import-list) nil)
  (set (make-local-variable 'company-elm-dependencies) nil)
  (set (make-local-variable 'company-elm-documentation) nil)

  (setq company-elm-import-list
        (append company-elm--default-import-list
                (company-elm--parse-imports-in-buffer)))

  (setq company-elm-dependencies
        (company-elm--read-exact-dependency-file
         (lambda () (error "Could not load dependencies."))))

  (let ((cache-dir (company-elm--get-cache-dir)))
    (when (not (f-exists? cache-dir))
      (message "Creating cache directory '%s'" cache-dir)
      (make-directory cache-dir t))
    (company-elm--load-dependencies cache-dir company-elm-dependencies)))

(defun company-elm--prefix ()
  "Get the current completion prefix"
  (let ((symbol (company-grab-symbol)))
    (echo "Found prefix: %s" symbol)
    symbol))

(defun company-elm--get-candidates (prefix)
  "Get completion candidates for elm"
  (echo "Getting candidates for %s" prefix)
  (mapcar (lambda (x) (cdr (assoc 'name x))) company-elm-documentation))

(defun company-elm-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-elm-backend))
    (prefix (company-elm--prefix))
    (init (company-elm--init))
    (candidates
     (company-elm--get-candidates arg))))


(defun company-elm-setup ()
  (interactive)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-elm-backend))
  (company-mode t))

(provide 'company-elm)
;;; company-elm.el ends here(require 'company)
