# WebApps

## Basics

### HTTP

#### Request

##### Structure

Line 1 - request method, uri, http version
> Method Request-URI HTTP-versio

Body

Headers - host, accept, authentication, user-agent...

Cookies - http is stateless-this serves as a state

#### Response

> HTTP-version Status-code Reason-phrase


#### Versions

/2 multiplexing, HTTPS "only"

/3 not widely supported

### HTML

html, head, title, /head, body, /body, /html

DOM Tree in infix serialization

WWW ~ Web of documents

Hyperlinks - resource references

Headings - section/article edit its size

Tables - care with colspan, rowspan

Meta elements

#### Forms

```html
<form method="get" action="http://www.example.org/newcustomer.php">
    Name: <input name="fullname" ... >
    Phone: <input name="phone" ... >
    Preferred delivery time: <input name="time" ... >
    Comments: <textarea name="comments"></textarea>
    <button type="submit">Submit Order</button>
</form>
```

Controls:
 - input
   - type
 - textarea
 - select
 - button

Attributes:
 - name
 - value

Other attributes
 - maxlength
 - pattern
 - required
 - placeholder
 - autocomplet

## CSS

Styles assigned to elements

- Inlined styles `<h1 style="color: red;">Red Heading</h1`
- `<style>` element in head
- linking from external file: `<link rel="stylesheet" type="text/css" href="styles.css">`

#### Selectors

 - by name
   - `p`
 - by id
   - `#theId`
 - by class
   - `.theClass`
 - universal
   - `*`

Combining:
 - `div.inf`
   - all div elements with class info
 - `h1#main`
   - h1 element with id main

Relative positions
- space
  - `E F`
    - F has ancestor E
- gt sign
  - `E > F`
    - E is (direct) parent of F
- plus
  - `E + F`
    - F immediately preceeded by E
- tilde
  - `E ~ F`
    - F preceeded by E

Aggregation:
- comma
  - `s1, s2`
    - use for s1 or s2

Pitfalls
- `ul li` vs `ul > li`
- `p.info` vs `p .info`
- `main ul, ol`

Pseudo-classes selectors:
![css1](notes-img/css1.png)

![css2](notes-img/css2.png)

![css3](notes-img/css3.png)

Cascading
1. Transition declarations (will be explained later)
2. Important user agent declarations
3. Important user declarations
4. Important override (by client-script) declarations
5. **Important author declarations**
6. Animation declarations (will be explained in adv. lectures)
7. Normal override declaration
8. **Normal author declarations**
9. Normal user declarations
10. Normal user agent declarations

Specificity either by the number the different selectors and if same, then the latest declared.

### Properties

Fonts, colorsD

#### Display

- block
- inline
- inline-block
- none
- list-item
- table, table-*

### Box model

![cssbox](notes-img/cssbox.png)


### Floating elements

`float:`
`clear`

### Filters

### Transformations

Some predefined, can use `matrix()`

### Transitions

For simple animations - between 2 states

![csstransition](notes-img/csstransition.png)

### Animations

Multiple states

### Media

Media Limitations
 - Restricting styles for particular visualization medium

Media Types
 - Select style sheets for particular media
   - screen, print, speech, …

Media Features (Properties)
 - Add additional conditions to the types
   - width, height
   - device-width, device-height
   - orientation, aspect-ratio

### Layout

Variable - `--varName`
 - access via: `var()`

Calculations
- `calc()`

Counters:
```css
body { counter-reset: chapter; }
h1 { counter-reset: section; }
h2 { counter-reset: subsection; }
h1::before {
  content: "Chapter " counter(chapter) ": ";
  counter-increment: chapter;
  color: #900;
}
h2::before {
  content: counter(chapter) "." counter(section) " ";
  counter-increment: section;
}
h3::before {
  content: counter(chapter) "." counter(section) "."
           counter(subsection) " ";
  counter-increment: subsection;
}

```

![csspositioning1](notes-img/csspositioning1.png)

### Basic layout

![csslayout](notes-img/csslayout.png)

### Modern layout

#### Flex box

Flow of UI controls

![cssflex](notes-img/cssflex.png)

#### Grid layout

Larger regular layouts

![grid](notes-img/cssgrid.png)

### Hacks

 - to center element: `margin-left: auto; margin-right: auto`

 - pre-`box-sizing`
   - martyoshka
     - double up `<div>`, outer width+margin and inner padding and border

### Responsive web

 - use relative widths in `%`
 - or use style sheets per different devices - use `media`

#### Frameworks

 - e.g. twitter bootstrap

### Issues with CSS

 - not DRY (dont repeat yourself) friendly
    - solution - preprocessing
      - LESS and SASS (Syntactically Awesome Stylesheets)

#### SASS (Syntactically Awesome Stylesheets)

Inheritance, variables, possible de-nesting of structures:
```css
nav {
    ul {
        margin: 0;
        li {
            display: inline-block;
        }
    }
    a {
        color: green;
    }
}
```

VS

```css

nav ul {
    margin...
}
nav ul li {
    display...
}
nav a {
    color...
}
```

## Web Applications Fundamentals and Server-side Technologies

CGI ~ common gateway interface - mostly server side

Scripting languages popular, already necessary on client side
 - drawback is speed

### Platforms

 - ASP.NET
   - WebForms
   - Razor - cshtml
   - MVC
 - JSP (Java Server Pages)
   - almost dead, lol (Spring boot, JSF)
 - Ruby on Rails
   - Convention over Configuration

### Integrated Web Server

![cgiintegrated](notes-img/cgiintegrated.png)

### Node.js
 - Server-side JS platform

## PHP

interleaving with HTML or standalone scripts

![phpinterleaving](notes-img/phpinterleaving.png)

### Language differences

![phplanguages](notes-img/phplanguages.png)

### Variables

Variables - `$thisIsAVariable`

#### Types

 - Scalar (`boolean`, `integer`, `float`, or `string`)
 - Compound (`array`, `object`)
 - Special (`resource`, `NULL`)

### String Literals

 - Single quoted strings ('text') – no special treatment
 - Double quoted strings ("text") – interpreted
   - Special escaped characters (\n, \r, \t, …)
   - Variables are replaced by their contents
   - `$a = 'foo'; $b = "Say $a\n";`

### Functions

![phpfunctions](notes-img/phpfunctions.png)

### HTTP Wrapper

`$_GET` / `$_POST` and others...

 - `REQUEST_METHOD` (GET/POST)
 - `SERVER_PROTOCOL` (HTTP/1.1)
 - `REQUEST_URI` (/index.php )
 - `REMOTE_ADDR`
 - `HTTP_ACCEPT`
 - `HTTP_ACCEPT_LANGUAGE`
 - `HTTP_ACCEPT_ENCODING`
 - `HTTP_ACCEPT_CHARSE`

![phppostex](notes-img/phppostex.png)

### Arrays

```php
$a = [ 'a', 4 => 'b', 'c' ];
$a[42] = 'd';
$a[] = 'e';
// [ 0=>'a', 4=>'b', 5=>'c', 42=>'d', 43=>'e' ]
```


### Variable variables

`$a = 'b';  $$a = 42;  // the same as $b = 42;`

### References

```php
$a = 1;
$b = &$a;
$b++;
echo $a;
// prints 2
```

### Functions

Type hinting in arguments - at least some runtime check.

No overloading, can be overridden

#### Indirect calling

```php
function foo($x, $y) { … }
$funcName = 'foo';
$funcName(42, 54); // the same as foo(42, 54)

call_user_func('foo', 42, 54)
```

#### Anonymous functions

Nameless, lambda eqviv but obsolete

```php
$fnc = function ($arg) { …body… };
$fnc(42)
```

### OOP in PHP

```php
class Foo {
    public $var = 0; // a member variable

    public function bar() { // a method
        echo $this->var;
    }
}
$instance = new Foo(); // create new instance
$instance->var = 42;
$instance->bar();
$instance = null;
```
#### References

![phpref](notes-img/phpref.png)

#### Classes

Implicit member declarations

The members can be oterated over - object treated as an array.

```php

class Foo {
    private $bar;
}
$foo = new Foo();
$foo->bar = 1;      // Error! 'bar' is private
$foo->barbar = 42;  // OK, new member is creat
```

`__constructor` VS `__destructor`

##### Member access

![phpmembers](notes-img/phpmembers.png)

##### Cloning

Shallow - `$b = $a`
Deep - explicit clone call - `$b = clone $a`

