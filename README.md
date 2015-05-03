# Company-web
Company-web is an alternative emacs plugin for autocompletion in html-mode, web-mode, jade-mode, slim-mode
and use data of [ac-html](https://github.com/cheunghy/ac-html).
It uses [company-mode](http://company-mode.github.io).

## Setup

Install `company`, 'ac-html' as dependency.

Add the following to your emacs-config:

```lisp
(require 'company)                                   ; load company mode
(require 'company-web-html)                          ; load company mode html backend
;; and/or
(require 'company-web-jade)                          ; load company mode jade backend
(require 'company-web-slim)                          ; load company mode slim backend

;; you may key bind, for example for web-mode:
(define-key web-mode-map (kbd "C-'") 'company-web-html)
```

Note: If you install `ac-html` via Melpa, as requirements `auto-complete` will be installed too,
however `company-web-html` don't require it.

Additionally you may want install `ac-html-csswatcher` and `ac-html-bootstrap`.

## Possible improvements of company-mode

```lisp
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
```

### Only use company-mode with company-web-html in web-mode
By default company-mode loads every backend it has. If you want to only have company-mode enabled in go-mode add the following to your emacs-config:

```lisp
(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))
```

### Color customization

```lisp
(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))
```

### Related projects
You may be interested in next projects:
- [ac-html-bootstrap](https://github.com/osv/ac-html-bootstrap) - Twitter:Bootstrap support for company-web/ac-html.
- [ac-html-csswatcher](https://github.com/osv/ac-html-csswatcher) - CSS/Less completion support for company-web/ac-html.
- [ac-html](https://github.com/cheunghy/ac-html) - same as company-web, but `auto-completion` completion framework.

