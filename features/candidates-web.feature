Feature company-web-html candidate
  Background:
    Given I turn on web-mode

  Scenario: web-mode tag candidates
    Given the buffer is empty
    And I insert:
    """
    <t
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "template"
    And company-web-html candidates contains "table"
    And company-web-html candidates not contains "class"
     
    When I insert "empl"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates are "("template")"
 
  Scenario: web-mode attribute candidates
    Given the buffer is empty
    And I insert:
    """
    <div cla
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates are "("class")"
    And company-web-html candidates not contains "div"

  Scenario: web-mode attribute value candidates
    Given the buffer is empty
    And I insert:
    """
    <div dir=""
    """
    And I press "<left>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "auto"

  Scenario: web-mode attribute style candidates
    Given the buffer is empty
    And I insert:
    """
    <div style=""
    """
    And I press "<left>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "animation"
    And company-web-html candidates contains "font"
    And company-web-html candidates not contains "div"

  Scenario: web-mode attribute style candidates usign "'" quote
    Given the buffer is empty
    And I insert:
    """
    <div style='
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "animation"
    And company-web-html candidates contains "font"
    And company-web-html candidates not contains "div"

  Scenario: web-mode attribute style candidates usign "'" quote and more attributes
    Given the buffer is empty
    And I insert:
    """
    <div dir='auto' sty
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "style"

  Scenario: web-mode attribute CSS candidates
    Given the buffer is empty
    And I insert:
    """
      <div style="font-family:  "
    """
    And I press "<left><left>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "Courier"
    And company-web-html candidates not contains "div"
    And company-web-html candidates not contains "color"
    And company-web-html candidates not contains "red"

    And I insert ";"
    And I execute company-web-html candidates command at current point
    Then company-web-jade candidates contains "color"
