;;; company-web-html.el --- company for html-mode & web-mode

;; Copyright (C) 2015 Olexandr Sydorchuck

;; Author: Olexandr Sydorchuck <olexandr.syd@gmail.com>
;; Keywords: html, company, auto-complete

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

(defcustom company-web-html-emmet-enable t
  "Enable emmet specified completion when `emmet-mode' active."
  :group 'company-web
  :type 'boolean)

;; html grabs
(defconst company-web-html-get-tag-re
  (concat "<[[:space:]]*\\(" company-web-selector "+\\)[[:space:]]+")
  "Regexp of html tag")

(defconst company-web-html-get-attribute-re
  (concat "[^[:alnum:]-]\\(" company-web-selector "+\\)=")
  "Regexp of html attribute")

(defun company-web-html-current-tag ()
  "Return current html tag user is typing on."
  (save-excursion
    (re-search-backward company-web-html-get-tag-re nil t)
    (match-string 1)))

(defun company-web-html-current-attribute ()
  "Return current html tag's attribute user is typing on."
  (save-excursion
    (re-search-backward company-web-html-get-attribute-re nil t)
    (match-string 1)))

(defconst company-web-html-tag-regexp
  (concat "<[[:space:]]*\\("
          company-web-selector
          "*\\)")
  "A regular expression matching HTML tags.")

(defconst company-web-html-attribute-regexp
  (concat "<[[:space:]]*" company-web-selector "[^>]*[[:space:]]+\\(.*\\)")
  "A regular expression matching HTML attribute.")

(defconst company-web-html-value-regexp
  (concat "\\w=[\"]\\(?:[^\"]+[ ;:]\\|\\)"
          "\\(" company-web-selector "*\\)")
  "A regular expression matching HTML attribute.")

;; emmet grabs
(defconst company-web-html-emmet-tag-regexp
  (concat "\\(?:^\\|[\t +>]+\\)" 
          "\\(" company-web-selector "*\\)")
  "A regular expression matching emmet's tags.")

(defconst company-web-html-emmet-class-regexp
  (concat  "\\(?:^\\|[\t +>]+\\)"
           ;; tag
           "\\(" company-web-selector "+\\|\\)"
           ;; skip #foo or .bar or .foo.bar.baz
           "[#.[:alnum:]-]*"
           ;; class
           "[.]\\(" company-web-selector "*\\)")
  "A regular expression matching emmet's class name.")

(defconst company-web-html-emmet-id-regexp
  (concat  "\\(?:^\\|[\t +>]+\\)"
           ;; tag
           "\\(" company-web-selector "+\\|\\)"
           ;; skip #foo or .bar or .foo.bar.baz
           "[#.[:alnum:]-]*"
           ;; class
           "[#]\\(" company-web-selector "*\\)")
  "A regular expression matching emmet's class name.")

(defun company-web-html-emmet-grab ()
  (and company-web-html-emmet-enable
       (-contains? minor-mode-list 'emmet-mode)
       (or
        (company-grab company-web-html-emmet-tag-regexp 1)
        (company-grab company-web-html-emmet-class-regexp 2)
        (company-grab company-web-html-emmet-id-regexp 2))))

(defun company-web-html-emmet-candidates()
   (when (and company-web-html-emmet-enable
              (-contains? minor-mode-list 'emmet-mode))
     (cond
      ((company-grab company-web-html-emmet-tag-regexp 1)
       (all-completions arg (company-web-candidates-tags)))
      ;; class (default for div tag)
      ((company-grab company-web-html-emmet-class-regexp 2)
       (let ((tag (company-grab company-web-html-emmet-class-regexp 1)))
         (if (string= "" tag)
             (setq tag "div"))
         (all-completions arg (company-web-candidates-attrib-values tag "class"))))
      ((company-grab company-web-html-emmet-id-regexp 2)
       (let ((tag (company-grab company-web-html-emmet-id-regexp 1)))
         (if (string= "" tag)
             (setq tag "div"))
         (all-completions arg (company-web-candidates-attrib-values tag "id")))))))

;;;###autoload
(defun company-web-html (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `html-mode' and `web-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-web-html))
    (ignore-case t)
    (duplicates nil)
    (prefix (and (or (derived-mode-p 'html-mode)
                     (derived-mode-p 'web-mode))
                 (or (company-grab company-web-html-value-regexp 1)
                     (company-grab company-web-html-tag-regexp 1)
                     (company-grab company-web-html-attribute-regexp 1)
                     (company-web-html-emmet-grab)
                     )))
    (candidates
     (cond

     ;;  ((company-grab company-web-html-value-regexp 1)
     ;;   (message "val"))
     ;;  ;; tag
     ;;  ((company-web-grab-not-in-string company-web-html-tag-regexp 1)
     ;;   (message "tag"))
     ;;  ;; attr
     ;;  ((company-web-grab-not-in-string company-web-html-attribute-regexp 1)
     ;;   (message "att"))
     ;;  ;; emmet
     ;; ((company-web-html-emmet-grab)
     ;;  (message "emmet"))


      ;; value
      ((company-grab company-web-html-value-regexp 1)
       (all-completions arg (company-web-candidates-attrib-values (company-web-html-current-tag)
                                                                  (company-web-html-current-attribute))))
      ;; tag
      ((company-web-grab-not-in-string company-web-html-tag-regexp 1)
       (all-completions arg (company-web-candidates-tags)))
      ;; attr
      ((company-web-grab-not-in-string company-web-html-attribute-regexp 1)
       (all-completions arg (company-web-candidates-attribute (company-web-html-current-tag))))
      ;; emmet
      ((company-web-html-emmet-grab)
       (company-web-html-emmet-candidates)))
)
    (annotation (company-web-annotation arg))
    (doc-buffer
     ;; No need grab for attribute value, attribute regexp will match enyway
     (cond
      ;; tag
      ((company-grab company-web-html-tag-regexp 1)
       (company-web-tag-doc arg))
      ;; attr
      ((company-grab company-web-html-attribute-regexp 1)
       (company-web-attribute-doc (company-web-html-current-tag) arg))))))

(provide 'company-web-html)
;;; company-web-html.el ends here
