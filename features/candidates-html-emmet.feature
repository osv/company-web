Feature company-web-html and emmet integration
  
    Background:
      Given I turn on html-mode
      And I turn on emmet-mode

  Scenario: html tag candidates
    Given the buffer is empty
    And I insert:
    """
    <foo bar>
      t
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html candidates contains "template"

  Scenario: No html emmet tag candidates in open tag
    Given the buffer is empty
    And I insert:
    """
    
     <a temp
    """
    And I execute company-web-html candidates command at current point
    Then company-web-html have no candidates

# TODO:  need   emmet-preview  test,  I   don't  know  how   to  check
# emmet-preview overlay for now :(
