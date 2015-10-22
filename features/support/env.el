(require 'f)

(defvar company-web-support-path
  (f-dirname load-file-name))

(defvar company-web-features-path
  (f-parent company-web-support-path))

(defvar company-web-root-path
  (f-parent company-web-features-path))

(add-to-list 'load-path company-web-root-path)

(require 'company-web)
(require 'espuds)
(require 'ert)
(require 'company-web-html)

(defvar company-cabal-test-prefix-output)
(defvar company-cabal-test-candidates-output)

(Before
 (setq company-web-test-prefix-output nil)
 (setq company-web-test-candidates-output nil)
 (switch-to-buffer
  (get-buffer-create "*company-web*"))
 (html-mode))
