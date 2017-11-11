Feature company-web-html and emmet integration
    Background:
      Given I turn on html-mode
      And I turn on emmet-mode

  Scenario: [emmet-mode] tag candidates
    Given the buffer is empty
    And I insert:
    """
    <foo bar>
      t
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "template"

  Scenario: [emmet-mode] attribute candidates
    Given the buffer is empty
    And I insert:
    """
    <foo bar>
      html>head+body.container>.row>.col-lg-12>form.form-horizontal[method="POST"ac]
    """
    And I press "<left>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "action"
    Then company-web-html candidates not contains "div"

  Scenario: [emmet-mode] second attribute candidates
    Given the buffer is empty
    And I insert:
    """
    <foo bar>
      form.form-inline[action="/api/foo"method=""
    """
    And I press "<left>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "POST"
    Then company-web-html candidates not contains "action"
    Then company-web-html candidates not contains "div"

  # TODO: Improve tag detect regexp to allow whitespace between [ ]
  # Scenario: emmet: two attributes
  #   Given the buffer is empty
  #   And I insert:
  #   """
  #   <foo bar>
  #     form.form-inline[action="/api/foo" method=""
  #   """
  #   And I press "<left>"
  #   And I execute company-web-html candidates command at current point
  #   Then company-web-html candidates contains "POST"
  #   Then company-web-html candidates not contains "action"
  #   Then company-web-html candidates not contains "div"

  Scenario: [emmet-mode] value candidates
    Given the buffer is empty
    And I insert:
    """
    <foo bar>
      div[dir=""]
    """
    And I press "<left><left>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "ltr"
    Then company-web-html candidates not contains "div"

  Scenario: [emmet-mode] value candidates if only emmet in buffer and quote is open
    Given the buffer is empty
    And I insert:
    """
      form[method=""
    """
    And I press "<backspace>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "POST"
    Then company-web-html candidates not contains "action"

  Scenario: [emmet-mode] No html emmet tag candidates in open tag
    Given the buffer is empty
    And I insert:
    """
    
     <a temp
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html have no candidates

    And I press "<backspace><backspace><backspace>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates not contains "template"
    Then company-web-html candidates contains "tabindex"

    # TODO:  need  emmet-preview test, I don't  know  how   to  check
    # emmet-preview overlay for now :(

    # TODO: Add doc test
