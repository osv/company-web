 (When "^I execute company-web-slim prefix command at current point$"
      (lambda ()
        (setq company-web-test-prefix-output
              (company-web-slim 'prefix))))

(When "^I execute company-web-slim post-completion with \"\\(.*\\)\""
      (lambda (str)
        (slim-mode)
        (company-web-slim 'post-completion str)))

(Then "^company-web-slim prefix is\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (equal company-web-test-prefix-output expected))))

(Then "^company-web-slim prefix none$"
      (lambda ()
        (should (not company-web-test-prefix-output))))

(When "^I execute company-web-slim candidates command at current point$"
      (lambda ()
        (let* ((tmp (or (company-web-slim 'prefix) 'stop))
               (prefix (if (consp tmp) (car tmp) tmp)))
          (when (not (eq prefix 'stop))
            (setq company-web-test-candidates-output
                  (mapcar (lambda (s) (substring-no-properties s))
                          (company-web-slim 'candidates prefix)))))))

(Then "^company-web-slim candidates are\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (equal company-web-test-candidates-output (read expected)))))

(Then "^company-web-slim candidates contains\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (member expected company-web-test-candidates-output))))

(Then "^company-web-slim candidates not contains\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (not (member expected company-web-test-candidates-output)))))
