Feature company-web-slim candidate

  Background:
    Given I turn on slim-mode

  Scenario: slim tag candidates
    Given the buffer is empty
    And I insert:
    """
    t
    """
    And I execute company-web-slim candidates command at current point
    Then company-web-slim candidates contains "template"
    And company-web-slim candidates contains "table"
    And company-web-slim candidates not contains "class"
     
    When I insert "empl"
    And I execute company-web-slim candidates command at current point
    Then company-web-slim candidates are "("template")"
 
  Scenario: slim attribute candidates
    Given the buffer is empty
    And I insert:
    """
      div cla
    """
    And I execute company-web-slim candidates command at current point
    Then company-web-slim candidates are "("class")"
    And company-web-slim candidates not contains "div"

  Scenario: slim attribute value candidates
    Given the buffer is empty
    And I insert:
    """
     div dir=""
    """
    And I press "<left>"
    And I execute company-web-slim candidates command at current point
    Then company-web-slim candidates contains "auto"

  Scenario: slim attribute style candidates
    Given the buffer is empty
    And I insert:
    """
      div style=""
    """
    And I press "<left>"
    And I execute company-web-slim candidates command at current point
    Then company-web-slim candidates contains "animation"
    And company-web-slim candidates contains "font"
    And company-web-slim candidates not contains "div"

  Scenario: slim multiple attributes
    Given the buffer is empty
    And I insert:
    """
      div dir="auto" c
    """
    And I execute company-web-slim candidates command at current point
    Then company-web-slim candidates contains "class"
    And company-web-slim candidates not contains "color"
    And company-web-slim candidates not contains "canvas"

  Scenario: slim attribute CSS candidates
    Given the buffer is empty
    And I insert:
    """
      div style="font-family:  "
    """
    And I press "<left><left>"
    And I execute company-web-slim candidates command at current point
    Then company-web-slim candidates contains "Courier"
    And company-web-slim candidates not contains "div"
    And company-web-slim candidates not contains "color"
    And company-web-slim candidates not contains "red"

    And I insert "; "
    And I execute company-web-slim candidates command at current point
    Then company-web-slim candidates contains "color"
"
