Feature company-web-html candidate

  Scenario: html tag candidates
    Given the buffer is empty
    When I insert:
    """
    <t
    """
    And I execute company-web candidates command at current point
    Then company-web-html candidates contains "template"
    And company-web-html candidates contains "table"
    And company-web-html candidates not contains "class"
     
    When I insert "empl"
    And I execute company-web candidates command at current point
    Then company-web-html candidates are "("template")"
 
  Scenario: html attribute candidates
    Given the buffer is empty
    When I insert:
    """
    <div cla
    """
    And I execute company-web candidates command at current point
    Then company-web-html candidates are "("class")"
    And company-web-html candidates not contains "div"

  Scenario: html attribute value candidates
    Given the buffer is empty
    When I insert:
    """
    <div dir=""
    """
    And I press "<left>"
    And I execute company-web candidates command at current point
    Then company-web-html candidates contains "auto"

  Scenario: html attribute style candidates
    Given the buffer is empty
    When I insert:
    """
    <div style=""
    """
    And I press "<left>"
    And I execute company-web candidates command at current point
    Then company-web-html candidates contains "animation"
    And company-web-html candidates contains "font"
    And company-web-html candidates not contains "div"
