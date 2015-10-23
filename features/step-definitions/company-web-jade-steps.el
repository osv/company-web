 (When "^I execute company-web-jade prefix command at current point$"
      (lambda ()
        (setq company-web-test-prefix-output
              (company-web-jade 'prefix))))

(When "^I execute company-web-jade post-completion with \"\\(.*\\)\""
      (lambda (str)
        (jade-mode)
        (company-web-jade 'post-completion str)))

(Then "^company-web-jade prefix is\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (equal company-web-test-prefix-output expected))))

(Then "^company-web-jade prefix none$"
      (lambda ()
        (should (not company-web-test-prefix-output))))

(When "^I execute company-web-jade candidates command at current point$"
      (lambda ()
        (let* ((tmp (or (company-web-jade 'prefix) 'stop))
               (prefix (if (consp tmp) (car tmp) tmp)))
          (when (not (eq prefix 'stop))
            (setq company-web-test-candidates-output
                  (mapcar (lambda (s) (substring-no-properties s))
                          (company-web-jade 'candidates prefix)))))))

(Then "^company-web-jade candidates are\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (equal company-web-test-candidates-output (read expected)))))

(Then "^company-web-jade candidates contains\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (member expected company-web-test-candidates-output))))

(Then "^company-web-jade candidates not contains\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (not (member expected company-web-test-candidates-output)))))
