Feature company-web-pug candidate
  Background:
    Given I turn on pug-mode

  Scenario: [pug-mode] tag candidates
    Given the buffer is empty
    And I insert:
    """
    t
    """
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates contains "template"
    And company-web-jade candidates contains "table"
    And company-web-jade candidates not contains "class"

    When I insert "empl"
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates are "("template")"

  Scenario: [pug-mode] attribute candidates
    Given the buffer is empty
    And I insert:
    """
      div(cla
    """
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates are "("class")"
    And company-web-jade candidates not contains "div"

  Scenario: [pug-mode] attribute value candidates
    Given the buffer is empty
    And I insert:
    """
     div(dir=""
    """
    And I press "<left>"
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates contains "auto"

  Scenario: [pug-mode] attribute value candidates quoted by "'"
    Given the buffer is empty
    And I insert:
    """
     div(dir='
    """
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates contains "auto"
    And I press "',"
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates contains "class"

  Scenario: [pug-mode] attribute style candidates
    Given the buffer is empty
    And I insert:
    """
      div(style=""
    """
    And I press "<left>"
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates contains "animation"
    And company-web-jade candidates contains "font"
    And company-web-jade candidates not contains "div"

  Scenario: [pug-mode] attribute CSS candidates
    Given the buffer is empty
    And I insert:
    """
      div(style="font-family:  ")
    """
    And I press "<left><left>"
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates contains "Courier"
    And company-web-jade candidates not contains "div"
    And company-web-jade candidates not contains "color"
    And company-web-jade candidates not contains "red"

    And I insert "; "
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates contains "color"

  Scenario: [pug-mode] complete style candidates when last char is '-'
    Given the buffer is empty
    And I insert:
    """
      div(style="font-")
    """
    And I press "<left><left>"
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates contains "font-family"
    And company-web-jade candidates not contains "font"
    And company-web-jade candidates not contains "div"
    And company-web-jade candidates not contains "color"
    And company-web-jade candidates not contains "red"
