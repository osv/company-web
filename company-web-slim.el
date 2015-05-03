;;; company-web-slim.el --- company for slim-mode

;; Copyright (C) 2015 Olexandr Sydorchuck

;; Author: Olexandr Sydorchuck <olexandr.syd@gmail.com>
;; Keywords: slim, company, auto-complete, javascript

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration:
;;

;;; Code:

(require 'company-web)

(defconst company-web/slim-get-tag-re
  (concat "^[\t ]*\\(" company-web-selector "+\\)")
  "Regexp of slim attribute or tag")

(defconst company-web/slim-get-attribute-re
  (concat "[^[:alnum:]-]\\(" company-web-selector "+\\) *=")
  "Regexp of slim attribute or tag")

(defun company-web/current-slim-tag ()
  "Return current slim tag user is typing on."
  (save-excursion
    (re-search-backward company-web/slim-get-tag-re nil t)
    (match-string 1)))

(defun company-web/current-slim-attribute ()
  "Return current slim tag's attribute user is typing on."
  (save-excursion
    (re-search-backward company-web/slim-get-attribute-re nil t)
    (match-string 1)))

(defconst company-web/slim-id-regexp
  (concat
   ;; tag or nil(div)
   "^ *\\(" company-web-selector "+\\|\\)"
   ;; classes ?
   "[.[:alnum:]-]*"
   ;; id?
   "#\\(" company-web-selector "*\\|\\)")

  "A regular expression matching Slim #id:

  #bar -> <div id=\"bar\">
or
  span#bar -> <span id=\"bar\">
.")

(defconst company-web/slim-class-regexp
  (concat
   ;; tag or nil(div)
   "^ *\\(" company-web-selector "+\\|\\)"
   "[#.[:alnum:]-]*"
   ;; last class
   "[.]\\(" company-web-selector "*\\)")

  "A regular expression matching Slim div's class:

  .foo -> <div class=\"foo\">
or
  span.foo
or
  #foo.baz -> <div id=\"foo\" class=\"baz\">
or
  span#foo.baz.bar
.")

(defconst company-web/slim-tag-regexp
  (concat "^[\t ]*\\(" company-web-selector "*\\)")
  "A regular expression matching Slim tags.")

(defconst company-web/slim-attribute-regexp
  (concat "\\(?:"                       ; attribute may start after tag and: "[", "(", "{"
          "[[({]"                       ;
          "\\|"                         ; or
          " +\\)"                       ; after whitespace
          "\\(" company-web-selector "*\\)")
  "A regular expression matching Slim attribute.")

(defconst company-web/slim-value-regexp
  (concat "\\w *= *[\"]\\(?:[^\"]+[ ]\\|\\)\\(" company-web-selector "*\\)")
  "A regular expression matching Slim attribute.")

;;;###autoload
(defun company-web-slim (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `slim-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-web-slim))
    (ignore-case t)
    (duplicates nil)
    (prefix (and (derived-mode-p 'slim-mode)
                 (or (company-grab company-web/slim-value-regexp 1)
                     (company-grab company-web/slim-tag-regexp 1)
                     (company-grab company-web/slim-id-regexp 2)
                     (company-grab company-web/slim-class-regexp 2)
                     (company-grab company-web/slim-attribute-regexp 1)
                     )))
    (candidates
     (cond
      ;; value
      ((company-grab company-web/slim-value-regexp 1)
       (all-completions arg (company-web-candidates-attrib-values (company-web/current-slim-tag)
                                                           (company-web/current-slim-attribute))))
      ;; class ".foo" or id "#bar"
      ((company-web-grab-not-in-string company-web/slim-id-regexp 1)
      (let ((tag (company-grab company-web/slim-id-regexp 1)))
         (if (string= "" tag)
             (setq tag "div"))
         (all-completions arg (company-web-candidates-attrib-values tag "id"))))

      ((company-web-grab-not-in-string company-web/slim-class-regexp 1)
       (let ((tag (company-grab company-web/slim-class-regexp 1)))
         (if (string= "" tag)
             (setq tag "div"))
         (all-completions arg (company-web-candidates-attrib-values tag "class"))))
      ;; tag
      ((company-web-grab-not-in-string company-web/slim-tag-regexp 1)
       (all-completions arg (company-web-candidates-tags)))
      ;; attr
      ((company-web-grab-not-in-string company-web/slim-attribute-regexp 1)
       (all-completions arg (company-web-candidates-attribute (company-web/current-slim-tag))))))
    (annotation (company-web-annotation arg))
    (doc-buffer
     ;; No need grab for attribute value, attribute regexp will match enyway
     (cond
      ((company-web-grab-not-in-string company-web/slim-id-regexp 1)
       (company-web-candidate-prop-doc arg))
      ((company-web-grab-not-in-string company-web/slim-class-regexp 2)
       (company-web-candidate-prop-doc arg))
      ;; tag
      ((company-grab company-web/slim-tag-regexp 1)
       (company-web-tag-doc arg))
      ;; attr
      ((company-grab company-web/slim-attribute-regexp 1)
       (company-web-attribute-doc (company-web/current-slim-tag) arg))))))

;;; company-web-slim.el ends here
