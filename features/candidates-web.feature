Feature company-web-html candidate Background:
  Background:
    Given I turn on web-mode

  Scenario: [web-mode] tag candidates
    Given the buffer is empty
    And I insert:
    """
    <t
    """
    And I execute company-web-html candidates command at current point
    And company-web-html-current-tag return "nil"
    Then company-web-html candidates contains "template"
    And company-web-html candidates contains "table"
    And company-web-html candidates not contains "class"

    When I insert "empl"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates are "("template")"

  Scenario: [web-mode] attribute candidates
    Given the buffer is empty
    And I insert:
    """
    <div cla
    """
    And company-web-html-current-tag return ""div""
    And company-web-html-current-attribute return "nil"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates are "("class")"
    And company-web-html candidates not contains "div"

  Scenario: [web-mode] second attribute candidate
    Given the buffer is empty
    And I insert:
    """
    <div tabindex cla
    """
    And company-web-html-current-tag return ""div""
    And company-web-html-current-attribute return "nil"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates are "("class")"
    And company-web-html candidates not contains "div"

  Scenario: [web-mode] attribute value candidates
    Given the buffer is empty
    And I insert:
    """
    <div dir=""
    """
    And I press "<left>"
    And company-web-html-current-attribute return ""dir""
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "auto"

  Scenario: [web-mode] attribute style candidates
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

  Scenario: [web-mode] attribute style candidates usign "'" quote
    Given the buffer is empty
    And I insert:
    """
    <div style='
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "background"
    Then company-web-html candidates contains "animation"
    And company-web-html candidates contains "font"
    And company-web-html candidates not contains "div"

  Scenario: [web-mode] Able to complete even if parent element has ":" char
    Given the buffer is empty
    And I insert:
    """
    <span foo="http://abc.com">
      <text class="" style=""
    """
    And I press "<left>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "background"
    Then company-web-html candidates contains "animation"
    And company-web-html candidates contains "font"
    And company-web-html candidates not contains "div"

  Scenario: [web-mode] attribute style candidates usign "'" quote and more attributes
    Given the buffer is empty
    And I insert:
    """
    <div dir='auto' sty
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "style"

  Scenario: [web-mode] Complete style candidates when last char is '-'
    Given the buffer is empty
    And I insert:
    """
      <div class="xyz" style="font-"
    """
    And I press "<left>"
    And company-web-html-current-tag return ""div""
    And company-web-html-current-attribute return ""style""
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "font-family"
    And company-web-html candidates not contains "font"
    And company-web-html candidates not contains "div"
    And company-web-html candidates not contains "color"
    And company-web-html candidates not contains "red"

  Scenario: [web-mode] Complete CSS candidates
    Given the buffer is empty
    And I insert:
    """
      <div class="xyz" style="font-family:  "
    """
    And I press "<left><left>"
    And company-web-html-current-tag return ""div""
    And company-web-html-current-attribute return ""style""
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "Courier"
    And company-web-html candidates not contains "div"
    And company-web-html candidates not contains "color"
    And company-web-html candidates not contains "red"

    And I insert " "
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "Courier"
    And company-web-html candidates not contains "div"
    And I insert ";"
    And I execute company-web-html candidates command at current point
    Then company-web-jade candidates contains "color"
    And I insert "color:"
    And I execute company-web-html candidates command at current point
    And company-web-html candidates contains "red"
    And company-web-html candidates not contains "div"

  Scenario: [web-mode] attribute CSS candidates when quote is not closed
    Given the buffer is empty
    And I insert:
    """
      <div style=""
    """
    And I press "<backspace>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "background"
    And company-web-html candidates not contains "div"
    And I insert "background:"
    And I execute company-web-html candidates command at current point
    Then company-web-jade candidates contains "red"
    And company-web-html candidates not contains "div"
    And I insert ";font:"
    And I execute company-web-html candidates command at current point
    Then company-web-jade candidates contains "cursive"
    And company-web-html candidates not contains "red"

  Scenario: [web-mode] attribute CSS candidates when quote is closed
    Given the buffer is empty
    And I insert:
    """
      <div style=""
    """
    And I press "<left>"
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "background"
    And company-web-html candidates not contains "div"
    And I insert "background:"
    And I execute company-web-html candidates command at current point
    Then company-web-jade candidates contains "red"
    And company-web-html candidates not contains "div"
    And I insert ";font:"
    And I execute company-web-html candidates command at current point
    Then company-web-jade candidates contains "cursive"
    And company-web-html candidates not contains "red"

  Scenario: [web-mode] attribute CSS candidates for last tag
    Given the buffer is empty
    And I insert:
    """
      <
        input
      />
        <
         div class="xyz" t
    """
    And company-web-html-current-tag return ""div""
    And company-web-html-current-attribute return ""class""
    And I execute company-web-html candidates command at current point
    # no input specified candidates
    And company-web-html candidates not contains "type"
    Then company-web-html candidates contains "tabindex"

  Scenario: [web-mode] bug 1: when tag length is 1 and space is 1
    Given the buffer is empty
    And I insert:
    """
      <link href="" rel="stylesheet" hreflang=""/>
      <!DOCTYPE html>
      <html><head>
          <link href="" rel="stylesheet" hreflang="d"/>
          <div style="background: whi" >
              <template class="xx">
                  <a dir=""
    """
    And I press "<left>"
    And company-web-html-current-tag return ""a""
    And company-web-html-current-attribute return ""dir""
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "ltr"
    And company-web-html candidates not contains "div"
