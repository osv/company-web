(When "^I execute company-web-html prefix command at current point$"
      (lambda ()
        (setq company-web-test-prefix-output
              (company-web-html 'prefix))))

(When "^I execute company-web-html post-completion with \"\\(.*\\)\""
      (lambda (str)
        (company-web-html 'post-completion str)))

(Then "^company-web-html prefix is\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (equal company-web-test-prefix-output expected))))

(Then "^company-web-html prefix none$"
      (lambda ()
        (should (not company-web-test-prefix-output))))

(When "^I execute company-web-html candidates command at current point$"
      (lambda ()
        (let* ((tmp (or (company-web-html 'prefix) 'stop))
               (prefix (if (consp tmp) (car tmp) tmp)))
          (when (not (eq prefix 'stop))
            (setq company-web-test-candidates-output
                  (mapcar (lambda (s) (substring-no-properties s))
                          (company-web-html 'candidates prefix)))))))

(Then "^company-web-html candidates are\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (equal company-web-test-candidates-output (read expected)))))

(Then "^company-web-html candidates contains\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (member expected company-web-test-candidates-output))))

(Then "^company-web-html have no candidates$"
      (lambda ()
        (should (equal company-web-test-candidates-output nil))))

(Then "^company-web-html candidates not contains\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (not (member expected company-web-test-candidates-output)))))

(Then "^company-web-html-current-attribute return\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (let* ((tag-info (company-web-html-current-tag))
               (tag-bound (cdr tag-info))
               (attribute (company-web-html-current-attribute tag-bound)))
          (should (equal attribute (read expected))))))

(Then "^company-web-html-current-tag return\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (equal (car (company-web-html-current-tag)) (read expected)))))
