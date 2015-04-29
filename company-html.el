;;; company-html.el --- Company version of ac-html, auto complete source for html tags and attributes

;; Copyright (C) 2015 Olexandr Sydorchuk

;; Author: Olexandr Sydorchuk <olexandr.syd@gmail.com>
;; Version: 0.1
;; Keywords: html, company
;; Package-Requires: ((company "0.8.0") (dash "2.8.0") (cl-lib "0.5.0"))
;; URL: https://github.com/osv/company-web

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
;; TODO: replace ac-html-source-dirs
;;
;;; Code:

(require 'company)
(require 'dash)
(require 'cl)

(defcustom company-web-framework-name-truncate-length 10
  "Truncation length for type framework-name"
  :type 'integer
  :group 'company-web)

(defvar company-web-string-check-faces '(font-lock-string-face web-mode-html-attr-value-face)
  "List of string faces to check.")

(defun company-web-load-list-from-file (filepath)
  "Return a list separated by \\n from FILEPATH."
  (with-current-buffer (find-file-noselect filepath)
    (unwind-protect
        (split-string (save-restriction
                        (widen)
                        (buffer-substring-no-properties
                         (point-min) (point-max)))
                      "\n" t)
      (kill-buffer))))

(defun company-web-is-point-in-string-face ()
  "t if text's face(s) at point is in `company-web-string-check-faces'."
  (let ((faces (get-text-property (point) 'face)))
    (if (listp faces)
        ;; slim-mode define list of string-face (bug), so intersect
        (intersection faces company-web-string-check-faces)
      (memq faces company-web-string-check-faces))))

(defun company-web-read-file (file-in-source-dir)
  "Return string content of FILE-IN-SOURCE-DIR from `ac-html-source-dirs'."
  (let ((file (cdr (nth 0 (company-web-all-files-named file-in-source-dir)))))
    ;; Just read from the first file.
    (when file
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun company-web-all-files-named (file-name)
  "Get a list of file named FILE-NAME in all directory specified by
 `ac-html-source-dirs'.

Returns an alist. car is source name, cdr is the file path."
  (let (return-files source-dir-path)
    (mapc (lambda (name-dir-cons-cell)
            (setq source-dir-path (cdr name-dir-cons-cell))
            (setq source-dir-path
                  (cond ((stringp source-dir-path) source-dir-path)
                        ((and (symbolp source-dir-path)
                              (boundp source-dir-path))
                         (symbol-value source-dir-path))
                        (t
                         (error "[companu-html] invalid element %s in\
 `ac-html-source-dirs'" source-dir-path))))
            (when source-dir-path
              (setq source-dir-path (expand-file-name file-name source-dir-path))
              (when (file-exists-p source-dir-path)
                (add-to-list 'return-files (cons (car name-dir-cons-cell) source-dir-path))
                )))
          ac-html-source-dirs)
    return-files))

(defun company-web-flatten (wtf)
  "Flatten WTF, into a list."
  (cond ((null wtf) nil)
        ((atom wtf) (list wtf))
        (t (append (company-web-flatten (car wtf))
                   (company-web-flatten (cdr wtf))))))

(defun company-web-make-candidate (framework-name items)
  "Make popup-item for each item with FRAMEWORK-NAME.

FRAMEWORK-NAME will be truncated to `companu-html-framework-name-truncate-length'.

ITEMS is a list of string where name and documentation are 
separated by one space.
Documentation newlines are escaped by \"\\n\".

If item have no inline documentation, DOCUMENTATION will be used.
DOCUMENTATION is string or function."
  (let ((annotation
         (truncate-string-to-width framework-name company-web-framework-name-truncate-length 0 nil nil)))
    (mapcar (lambda (item)
              (if (string-match "\\(.*?\\) \\(.*\\)" item)
                  (propertize (match-string 1 item)
                              'annotation annotation
                              'doc (replace-regexp-in-string "\\\\n" "\n"
                                                             (match-string 2 item)))
                (propertize item
                            'annotation annotation)))
            items)))

;; candidate getters
(defun company-web-candidates-tags ()
  (company-web-flatten
   (mapcar (lambda (source-name-and-file-path)
             (company-web-make-candidate
              (car source-name-and-file-path)
              (company-web-load-list-from-file (cdr source-name-and-file-path))))
           (company-web-all-files-named "html-tag-list"))))

(defun company-web-candidates-attribute (tag)
  "Attribute candidates of TAG."
  (let* ((items (mapcar (lambda (plist-framwork-and-file)
                          (company-web-make-candidate
                           (concat (car plist-framwork-and-file) ", G")
                           (company-web-load-list-from-file (cdr plist-framwork-and-file))))
                        (company-web-all-files-named "html-attributes-list/global"))))
    (add-to-list 'items
                 (mapcar (lambda (plist-framwork-and-file)
                           (company-web-make-candidate
                            (car plist-framwork-and-file)
                            (company-web-load-list-from-file (cdr plist-framwork-and-file))))
                         (company-web-all-files-named (concat "html-attributes-list/" tag))))
    (company-web-flatten items)))

(defun company-web-annotation (candidate)
  "Return type annotation for chosen CANDIDATE."
  (concat
   (unless company-tooltip-align-annotations " -> ")
   (get-text-property 0 'annotation candidate)))

(defun company-web-tag-doc (candidate)
  "Return documentation for chosen CANDIDATE.
Property of doc CANDIDATE or load file from `html-tag-short-docs/CANDIDATE'"
  (let ((doc (get-text-property 0 'doc candidate)))
    (unless doc
      (let ((doc-file (cdr (car (company-web-all-files-named (concat "html-tag-short-docs/" candidate))))))
        (when doc-file
          (setq doc (company-web-read-file doc-file)))))
    (when doc
      (company-doc-buffer doc))))


(defun company-web-attribute-doc (tag candidate)
  "Return documentation for chosen CANDIDATE.
Property of doc CANDIDATE or load file from `html-attributes-short-docs/global-CANDIDATE' or
`html-attributes-short-docs/TAG-CANDIDATE'"
  (let ((doc (get-text-property 0 'doc candidate)))
    (unless doc
      (let ((doc-file (cdr (car (company-web-all-files-named (concat "html-attributes-short-docs/" tag "-" candidate))))))
        (when doc-file
          (setq doc (company-web-read-file doc-file)))))
    (unless doc
      (let ((doc-file (cdr (car (company-web-all-files-named (concat "html-attributes-short-docs/global-" candidate))))))
        (when doc-file
          (setq doc (company-web-read-file doc-file)))))
    (when doc
      (company-doc-buffer doc))))

(defun company-web/current-html-tag ()
  "Return current html tag user is typing on."
  (save-excursion
    (re-search-backward "<\\(\\w+\\)[[:space:]]+" nil t)
    (match-string 1)))

(defconst company-web/html-tag-regexp
  (concat "<[[:space:]]*\\(\\w*\\)")
  "A regular expression matching HTML tags.")

(defconst company-web/html-attribute-regexp
  (concat "<\\w+[[:space:]]+\\(\\w*\\)")
  "A regular expression matching HTML attribute.")

;;;###autoload
(defun company-web-html (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `html-mode' and `web-mode'."
  (interactive (list 'interactive))
;;  (message "start %S %S" command arg)
  (cl-case command
    (interactive (company-begin-backend 'company-web-html))
    (prefix (and (or (derived-mode-p 'html-mode)
                     (derived-mode-p 'web-mode))
                 (or (company-grab company-web/html-attribute-regexp 1)
                     (company-grab company-web/html-tag-regexp 1)
                     )))
    (candidates
     (cond
      ;; tag
      ((and (not (company-web-is-point-in-string-face))
            (company-grab company-web/html-tag-regexp 1))
       (all-completions arg (company-web-candidates-tags)))
      ;; attr
      ((and (not (company-web-is-point-in-string-face))
            (company-grab company-web/html-attribute-regexp 1))
       (all-completions arg (company-web-candidates-attribute (company-web/current-html-tag))))))
    (annotation (company-web-annotation arg))
    (doc-buffer
     (cond
      ;; tag
      ((company-grab company-web/html-tag-regexp 1)
       (company-web-tag-doc arg))
      ((company-grab company-web/html-attribute-regexp 1)
       (company-web-attribute-doc (company-web/current-html-tag) arg))))))
