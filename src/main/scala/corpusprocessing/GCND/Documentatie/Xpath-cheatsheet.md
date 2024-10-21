Cheat Sheet
To make your life easier, we've compiled a handy cheat sheet with quick-reference tables for common expressions, functions, and operators.

This cheat sheet is your go-to resource when working with XPath in your automation testing or web scraping projects.

Basic XPath Syntax:
* / - Selects from the root node.
* // - Selects nodes anywhere in the document.
* . - Represents the current node.
* .. - Represents the parent of the current node.


Selectors:

* element - Selects all elements with the given name.
* @attribute - Selects the value of the specified attribute.
* `*`  Selects all child elements.
* text() - Selects the text within an element.
* [predicate] - Adds a condition to filter nodes.

Predicates:
* [name='value'] - Selects nodes with the specified attribute value.
* [position()] - Selects nodes based on their position.
* [last()] - Selects the last node of a given type.
* [contains(@attribute, 'value')] - Selects nodes with attribute values containing 'value'.
* [not(predicate)] - Negates a condition.

Axes:
* ancestor:: - Selects all ancestors.
* ancestor-or-self:: - Selects ancestors and the current node.
* child:: - Selects all children.
* descendant:: - Selects all descendants.
* descendant-or-self:: - Selects descendants and the current node.
* following:: - Selects all following nodes.
* following-sibling:: - Selects following siblings.
* parent:: - Selects the parent node.
* preceding:: - Selects all preceding nodes.
* preceding-sibling::- Selects preceding siblings.
* self:: - Selects the current node.

Operators:
* = - Equal to.
* != - Not equal to.
* < - Less than.
* <= - Less than or equal to.
* > - Greater than.
* >= - Greater than or equal to.
* and - Logical AND.
* or - Logical OR.
* not - Logical NOT.

Functions (Examples):

* name() - Returns the name of the current node.
* count(nodes) - Returns the number of nodes in the node-set.
* concat(string1, string2) - Concatenates two strings.
* substring(string, start, length) - Returns a substring.
* contains(string, substr) - Checks if a string contains a substring.
* normalize-space(string) - Removes leading/trailing whitespace and collapses spaces.

Examples:

* /bookstore/book - Selects all book elements in the root bookstore.
* //title[text()='XPath'] - Selects title elements with text 'XPath' anywhere in the document.
* //*[@id='myId'] - Selects elements with the attribute id equal to 'myId'.
* /bookstore/book[position()=1] - Selects the first book element.
* //div[@class='highlight']//p - Selects p elements within div with class 'highlight'.
* //a[contains(@href, 'example.com')] - Selects a elements with 'example.com' in the href attribute.
