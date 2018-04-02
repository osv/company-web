Feature company-web-jade candidate

  Background:
    Given I turn on jade-mode

  Scenario: [jade-mode] tag candidates
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

  Scenario: [jade-mode] attribute candidates
    Given the buffer is empty
    And I insert:
    """
      div(cla
    """
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates are "("class")"
    And company-web-jade candidates not contains "div"

  Scenario: [jade-mode] attribute value candidates
    Given the buffer is empty
    And I insert:
    """
     div(dir=""
    """
    And I press "<left>"
    And I execute company-web-jade candidates command at current point
    Then company-web-jade candidates contains "auto"

  Scenario: [jade-mode] attribute value candidates quoted by "'"
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

  Scenario: [jade-mode] attribute style candidates
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

  Scenario: [jade-mode] attribute CSS candidates
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

  Scenario: [jade-mode] complete style candidates when last char is '-'
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
