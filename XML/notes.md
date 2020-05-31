# Technologie XML

<!-- toc -->

- [Progress](#Progress)
  * [Presentations](#Presentations)
  * [Videos](#Videos)
  * [Classes](#Classes)
- [Intro](#Intro)
  * [Well-formed XML document](#Well-formed-XML-document)
    + [Prolog](#Prolog)
  * [XML Technologies](#XML-Technologies)
- [DTD](#DTD)
  * [Valid XML document](#Valid-XML-document)
    + [Structure](#Structure)
      - [Tags](#Tags)
      - [Element](#Element)
      - [Attribute](#Attribute)
        * [Data Types](#Data-Types)
        * [Presence](#Presence)
      - [Entities](#Entities)
        * [Character Entities](#Character-Entities)
        * [General Entity](#General-Entity)
        * [Parameter Entity](#Parameter-Entity)
        * [Condional Sections](#Condional-Sections)
- [XML](#XML)
  * [Namespaces](#Namespaces)
    + [Examples](#Examples)
  * [Data Model - InfoSet](#Data-Model---InfoSet)
    + [Item properties - element](#Item-properties---element)
    + [Post Schema Validation Infoset (PSVI)](#Post-Schema-Validation-Infoset-PSVI)
  * [Formats](#Formats)
    + [XHTML](#XHTML)
      - [XHTML vs. HTML](#XHTML-vs-HTML)
      - [XHTML DTD](#XHTML-DTD)
    + [SVG](#SVG)
    + [XForms](#XForms)
    + [OOXML](#OOXML)
    + [WSDL](#WSDL)
      - [Example](#Example)
        * [Parameters of Operations](#Parameters-of-Operations)
        * [Operation](#Operation)
        * [Way of Communication](#Way-of-Communication)
        * [Service Definition](#Service-Definition)
- [XPath](#XPath)
        * [Example](#Example-1)
  * [Expression](#Expression)
    + [Evaluation](#Evaluation)
    + [Formally](#Formally)
      - [Axes](#Axes)
      - [Node-test](#Node-test)
        * [Examples](#Examples-1)
      - [Predicates](#Predicates)
        * [Examples](#Examples-2)
      - [Built-in Functions](#Built-in-Functions)
  * [Advanced XPath](#Advanced-XPath)
  * [Data Types](#Data-Types-1)
    + [XPath 2.0 – Data Model](#XPath-20-–-Data-Model)
      - [Sequence](#Sequence)
      - [Iteration](#Iteration)
      - [Quantifiers](#Quantifiers)
      - [Merging](#Merging)
      - [Comparison](#Comparison)
      - [Conditions](#Conditions)
- [SQL/XML](#SQLXML)
  * [Functions for Data Publication](#Functions-for-Data-Publication)
    + [XMLELEMENT](#XMLELEMENT)
      - [Namespaces](#Namespaces-1)
    + [XMLATTRIBUTES](#XMLATTRIBUTES)
    + [XMLFOREST](#XMLFOREST)
    + [XMLCONCAT](#XMLCONCAT)
    + [XMLAGG](#XMLAGG)
    + [XML Data Type and Querying](#XML-Data-Type-and-Querying)
      - [XMLQUERY](#XMLQUERY)
      - [XMLTABLE](#XMLTABLE)
      - [XMLEXISTS](#XMLEXISTS)
      - [Other Constructs](#Other-Constructs)
- [XML Schema](#XML-Schema)
  * [Advantages](#Advantages)
  * [Disadvantages = advantages](#Disadvantages--advantages)
  * [Basics](#Basics)
        * [Examples](#Examples-3)
    + [Root-element](#Root-element)
    + [How to work](#How-to-work)
  * [Basic components](#Basic-components)
    + [Simple Data Types](#Simple-Data-Types)
      - [Primitive Data Types](#Primitive-Data-Types)
      - [User defined simple types](#User-defined-simple-types)
        * [Restriction](#Restriction)
        * [List](#List)
        * [Union](#Union)
      - [Global VS Local definition](#Global-VS-Local-definition)
    + [Attributes](#Attributes)
        * [Examples](#Examples-4)
    + [Elements](#Elements)
    + [Complex Data Types](#Complex-Data-Types)
      - [Simple content](#Simple-content)
      - [Sequence](#Sequence-1)
      - [Choice](#Choice)
      - [All](#All)
        * [Version differences](#Version-differences)
      - [Group](#Group)
        * [References](#References)
      - [Complex content](#Complex-content)
    + [Invariants - assert](#Invariants---assert)
    + [Attritbute group](#Attritbute-group)
  * [Advanced components](#Advanced-components)
    + [Namespaces](#Namespaces-2)
    + [Include](#Include)
    + [Import](#Import)
  * [Identity Restriction](#Identity-Restriction)
    + [Unique](#Unique)
    + [Key](#Key)
    + [Keyref](#Keyref)
  * [Implicit Substitutability (Substitutability of Data Types)](#Implicit-Substitutability-Substitutability-of-Data-Types)
  * [Substitution Groups (Substitutability of Elements)](#Substitution-Groups-Substitutability-of-Elements)
  * [Wildcards](#Wildcards)
  * [Notation](#Notation)
  * [Annotation](#Annotation)
- [XSLT](#XSLT)
  * [Basic Principles](#Basic-Principles)
  * [Basics](#Basics-1)
  * [Templates](#Templates)
    + [Unnamed](#Unnamed)
    + [Named](#Named)
    + [XSLT script](#XSLT-script)
      - [How does it work](#How-does-it-work)
    + [Body](#Body)
      - [1 Creating Elements/Attributes](#1-Creating-ElementsAttributes)
      - [2 Text Nodes](#2-Text-Nodes)
      - [3 Input Data](#3-Input-Data)
      - [4 Calling Other Templates](#4-Calling-Other-Templates)
        * [Element xsl:apply-templates](#Element-xslapply-templates)
        * [Element xsl:call-template](#Element-xslcall-template)
      - [5 Variables and Parameters](#5-Variables-and-Parameters)
      - [6 Repetition](#6-Repetition)
      - [7 Branching](#7-Branching)
        * [Conditions](#Conditions-1)
        * [General](#General)
        * [Recursion (examples)](#Recursion-examples)
  * [Implicit Templates](#Implicit-Templates)
  * [XSLT programming – Two Approaches](#XSLT-programming-–-Two-Approaches)
  * [Advanced XSLT](#Advanced-XSLT)
    + [XSLT 1.0](#XSLT-10)
      - [Sorting](#Sorting)
      - [Keys](#Keys)
      - [Modes](#Modes)
      - [Combinations of Scripts](#Combinations-of-Scripts)
      - [Copies of Nodes](#Copies-of-Nodes)
    + [XSLT 2.0](#XSLT-20)
      - [Output and Input](#Output-and-Input)
      - [Grouping of Nodes](#Grouping-of-Nodes)
      - [User-defined Functions](#User-defined-Functions)
    + [XSLT 3.0](#XSLT-30)
      - [Streaming](#Streaming)
      - [Higher-Order Functions](#Higher-Order-Functions)
- [Schemas](#Schemas)
  * [Best Practices](#Best-Practices)
    + [Basic Recommendations](#Basic-Recommendations)
    + [Elements vs. Attributes](#Elements-vs-Attributes)
  * [Namespaces](#Namespaces-3)
  * [XML Schema Versioning](#XML-Schema-Versioning)
    + [Takeaways](#Takeaways)
  * [Extensible Content Types](#Extensible-Content-Types)
  * [Conclusion](#Conclusion)
- [RELAX NG](#RELAX-NG)
  * [General features](#General-features)
  * [XML vs. Compact Syntax](#XML-vs-Compact-Syntax)
  * [Data Types](#Data-Types-2)
        * [Example](#Example-2)
  * [Conclusion](#Conclusion-1)
- [Schematron](#Schematron)
  * [Structure](#Structure-1)
  * [Validation](#Validation)
  * [Combination with XML Schema](#Combination-with-XML-Schema)
  * [Combination with RELAX NG](#Combination-with-RELAX-NG)
- [XQuery](#XQuery)
  * [Constructs](#Constructs)
    + [Constructors](#Constructors)
    + [FLWOR](#FLWOR)
      - [Parts](#Parts)
      - [Use](#Use)
    + [Conditions](#Conditions-2)
    + [Quantifiers](#Quantifiers-1)
    + [Functions](#Functions)
        * [Examples](#Examples-5)
      - [Imports](#Imports)
    + [Comparison](#Comparison-1)
      - [Value](#Value)
      - [General (Group)](#General-Group)
      - [Node](#Node)
    + [Integrity Constraints](#Integrity-Constraints)
  * [Support for Schemas](#Support-for-Schemas)
  * [Semantics](#Semantics)
  * [Advanced XQuery](#Advanced-XQuery)
    + [XQuery Update Facility 1.0](#XQuery-Update-Facility-10)
    + [(Dotazovaci vyrazy vs) Aktualizacni vyrazy](#Dotazovaci-vyrazy-vs-Aktualizacni-vyrazy)
      - [Construct insert](#Construct-insert)
      - [Construct delete](#Construct-delete)
      - [Construct replace](#Construct-replace)
      - [Construct replace value of](#Construct-replace-value-of)
      - [Other constructs](#Other-constructs)
    + [Special Namespaces](#Special-Namespaces)
    + [XQuery Data Model](#XQuery-Data-Model)
      - [Atomic Values](#Atomic-Values)
      - [Nodes](#Nodes)
      - [Result](#Result)
    + [XQuery 3.0](#XQuery-30)
- [XQuery vs. XSLT](#XQuery-vs-XSLT)
  * [Examples](#Examples-6)
  * [Takeawys](#Takeawys)
- [Parsers](#Parsers)
  * [Types of parsers](#Types-of-parsers)
  * [SAX](#SAX)
    + [Interface - ContentHandler](#Interface---ContentHandler)
      - [startElement()](#startElement)
      - [Interface attributes](#Interface-attributes)
      - [characters()](#characters)
      - [ignorableWhitespace()](#ignorableWhitespace)
      - [setDocumentLocator()](#setDocumentLocator)
    + [Initialization](#Initialization)
    + [StAX](#StAX)
      - [Initialization](#Initialization-1)
      - [XMLEventReader](#XMLEventReader)
      - [XMLEventWriter](#XMLEventWriter)
      - [XMLStreamReader](#XMLStreamReader)
      - [XMLStreamWriter](#XMLStreamWriter)
  * [DOM](#DOM)
    + [Initialization](#Initialization-2)
    + [Hierarchy](#Hierarchy)
    + [Interface Node](#Interface-Node)
      - [Examples of processing Child Nodes vs. Attributes](#Examples-of-processing-Child-Nodes-vs-Attributes)
    + [Interface Document](#Interface-Document)
    + [Interface Element](#Interface-Element)
    + [Interface Attr](#Interface-Attr)
    + [Interface CharacterData](#Interface-CharacterData)
    + [Interface Text](#Interface-Text)
    + [Interface ProcessingInstruction (PI)](#Interface-ProcessingInstruction-PI)
    + [Interface Notation](#Interface-Notation)
    + [Interface Entity](#Interface-Entity)
    + [Interface DocumentType](#Interface-DocumentType)
    + [Other Interfaces](#Other-Interfaces)
    + [Other classes](#Other-classes)
  * [JAXP](#JAXP)
- [XML Databases](#XML-Databases)
  * [Documents vs. Databases](#Documents-vs-Databases)
  * [Documents and Structured Data](#Documents-and-Structured-Data)
  * [Classification of XML Documents](#Classification-of-XML-Documents)
    + [Implementation Approaches](#Implementation-Approaches)
    + [Data-oriented XML Documents](#Data-oriented-XML-Documents)
      - [Techniques](#Techniques)
    + [Document-oriented XML Documents](#Document-oriented-XML-Documents)
      - [Techniques](#Techniques-1)
  * [Numbering Schemas](#Numbering-Schemas)
    + [Dietz Numbering](#Dietz-Numbering)
    + [Depth-first (DF) Numbering](#Depth-first-DF-Numbering)
    + [ORDPATH](#ORDPATH)
    + [XML Databases - Mapping Methods](#XML-Databases---Mapping-Methods)
    + [Generic Methods](#Generic-Methods)
      - [Table-based Mapping](#Table-based-Mapping)
      - [Generic-tree Mapping](#Generic-tree-Mapping)
      - [Structure-centred Mapping](#Structure-centred-Mapping)
      - [Simple-path Mapping](#Simple-path-Mapping)
    + [Schema-driven Mapping](#Schema-driven-Mapping)
      - [Fixed methods](#Fixed-methods)
        * [Basic, Shared and Hybrid](#Basic-Shared-and-Hybrid)
      - [Flexible methods](#Flexible-methods)
        * [LegoDB mapping](#LegoDB-mapping)
        * [Hybrid object-relational mapping](#Hybrid-object-relational-mapping)
    + [User-defined Mapping](#User-defined-Mapping)
      - [Example – system XCacheDB](#Example-–-system-XCacheDB)
  * [Current State of the Art of XML Databases](#Current-State-of-the-Art-of-XML-Databases)

<!-- tocstop -->


---
## Progress

### Presentations

Friday:
- [x] Lec 1
- [x] Lec 2
- [x] Lec 3
- [x] Lec 4

### Videos

Saturday:
- [x] Lec 5
- [x] Lec 6
- [x] Lec 7
- [x] Lec 8
- [x] Lec 9

Sunday:
- [x] Lec 10
- [x] Lec 11
- [x] Lec 12

---

### Classes

Monday:
- [ ] DTD
- [ ] XPath
- [ ] XML Schema
- [ ] XSLT
- [ ] XQuery
- [ ] SAX, DOM

---
## Intro

Motivation was exchange of information

XML has:
- Format
  - XML +  version
- Encoding 
  - by default ISO 10646 (Unicode) 
  - to communicate with the whole world we can use UTF8
  - for the Czech language we have ISO-8859-2 or Windows-125

XML ~ eXtensible Markup Language

![f5e750d5.png](attachments/15e42996-b386-4d59-87af-840ece336296/f5e750d5.png)

XML does not deal with data presentation
- It enables to tag parts of the data 
- The meaning of the tags depends on the author
  - Presentation is one possible example

### Well-formed XML document

- It has introductory prolog
- Start and end tags nest properly
  - Each element has a start and an end tag
  - Corresponding tags have the same name (case sensitivity)
    - `<a></A>`
  - Pairs of tags do not cross
    - `<a><b></a></b>`
  - The whole document is enclosed in a single root element

#### Prolog 

- An information for the SW that it works with an XML document
- It must contain declaration of XML version
  - We have 1.0 and 1.1
- It can contain information about encoding and if the document is standalone
- Examples:
  - Version: `<?xml version="1.1"?>`
  - Encoding other than UTF-8: `<?xml version="1.1" encoding="iso-8859-2"?>`
  - Standalone document: `<?xml version="1.1" standalone="yes"?>`

### XML Technologies

~ a family of technologies to process XML data

- Description of allowed content, data interface, parsing of data, information extraction (querying), transformation into other formats, ...
  - W3C (WWW Consortium) standards
- Efficient implementation of the standards
  - Parsers, validators, query evaluators, XSL transformers, data persistence, … 

![69f7850f.png](attachments/15e42996-b386-4d59-87af-840ece336296/69f7850f.png)

---
## DTD

~ Document Type Definition

Describes the structure (grammar) of an XML document
- Using regular expressions

Well-formedness is not enough
- We need to restrict the set of tags and their content

### Valid XML document

~ well-formed XML document corresponding to a given grammar

- There are also other languages (not only DTD) – XML Schema, Schematron, RELAX NG, …

#### Structure
- can be internal
  - `<!ELEMENT root-element (#PCDATA)>`
- external
  - `<!DOCTYPE root-element SYSTEM "root-element.dtd">`
  - `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">`
```xml
<?xml version="1.0" ?>
<!DOCTYPE root-element [
  ...
]> 
<root-element>
  ...
</root-element>
```

##### Tags

- Document type declaration `<!DOCTYPE … >`
- Element type declaration `<!ELEMENT … >`
- Declaration of a list of attributes `<!ATTLIST … >` 
- Declaration of an entity `<!ENTITY … >` 
- Declaration of a notation `<!NOTATION … >`

##### Element

Element name + declaration of allowed content:
- empty
- any
- text
- mixed
- element

Uses regular expressions:

| char | meaning               |
| ---- | --------------------- |
| `,`  | sequence              |
| `|`  | selection             |
| `?`  | iteration (0 or 1)    |
| `+`  | iteration (1 or more) |
| `*`  | iteration (0 or more) |

```dtd
<!ELEMENT parent (child*)> 

<parent>
  <child> ... </child> 
  <child> ... </child> 
  ... 
</parent>
```

- Empty content `<!ELEMENT attachment EMPTY>`
- Any content `<!ELEMENT container ANY>`
- Text content `<!ELEMENT surname (#PCDATA)>`
- Mixed content `<!ELEMENT text (#PCDATA | it | bold)*> ` 
  - the `*` is not regular exp.
- Element content `<!ELEMENT message (address, text)>`

##### Attribute

```dtd
<!ATTLIST person number ID #REQUIRED
                 employed CDATA #FIXED "yes"
                 holiday (yes|no) "no">
```

###### Data Types

- CDATA – arbitrary text string 
- Enumeration of values
- ID – unique identifier (within the content of the document), it must be a string of letters, numbers and characters „-“, „\_“, „:“, „.“, preferably in ASCII, it must start with a letter or character „\_“
- IDREF – reference to an ID of any element in the document 
- IDREFS – list of references (delimited with white spaces) to IDs
- NMTOKEN – string similar to ID, not unique, can start with a number
- NMTOKENS – list of NMTOKENs
- ENTITY – link to an external entity
- ENTITIES – list of links to external entities

###### Presence

- #REQUIRED
  - the attribute is compulsory
- #IMPLIED
  - the attribute is optional
- #FIXED
  - the attribute has a fixed value

##### Entities

~ association of a name and a value which is later (repeatedly) used

- Classification 1:
  - Parsed = the text which replaces the link to an entity becomes a part of the document
    - We refer using references
  - Unparsed = a resource which can contain anything (e.g. binary data = an image, a video)
    - We refer using attributes of type ENTITY/ENTITIES
    - It must be associated with a notation
- Classification 2:
  - General – in XML documents
  - Parameter – in DTDs
- Classification 3: 
  - Internal 
  - External


###### Character Entities

- A possibility to insert a character with any code
  - Hexadecimal or decimal
  - `Solve inequality 3x &#x3C; 5`
- Pre-defined entities for special characters
  - `Solve inequality 3x &lt; 5`
  - Predefined:
    - & … amp
    - < … lt
    - \> … gt 
    - ‘ … apos 
    - “ … quot

###### General Entity

- Internal (hence of course parsed) entity
  - Usage: Repeating parts of XML documents
  - `<!ENTITY status "working draft">`
  - `<note>The current status of the document is &status;</note>`
- External parsed entity
  - Usage: Modularity of XML documents
  - `<!ENTITY xml-serial SYSTEM "xml-serial.txt">`
- External unparsed entity
  - Usage: Reference to non-XML data
```xml
<?xml version="1.0" encoding="windows-1250"?>
<!DOCTYPE message [
  <!NOTATION avi SYSTEM
    "C:/Program Files/Video Player/Player.exe">
  <!ENTITY video SYSTEM "video.avi" NDATA avi>
  <!ELEMENT video-holiday (#PCDATA)>
  <!ATTLIST video-holiday src ENTITY>
]>

<message>
  I enclose the <video-holiday src="video">video</video-holiday> from holiday.
</message>
```

###### Parameter Entity

- Internal entity
  - Usage: repeating parts of DTDs
```dtd
<!ELEMENT rental (car*)>
<!ENTITY % attributes "color (blue|white|black) #REQUIRED speed (high|low) #IMPLIED" >
<!ELEMENT car (#PCDATA)>
<!ATTLIST car %attributes; >
<!ELEMENT motorcycle (#PCDATA)>
<!ATTLIST motorcycle %attributes; >
<!ELEMENT bike (#PCDATA)> <!ATTLIST bike %attributes; >
```
- External entity
  - Usage: Modularity of DTDs
```dtd
<!ENTITY % ISOLat2 SYSTEM "iso-pub.ent">
...
%ISOLat2;
..
```


###### Condional Sections

```dtd
<!ENTITY % draft 'INCLUDE' > 
<!ENTITY % final 'IGNORE' >

<![%draft;[
  <!ELEMENT book (comments*, title, body, supplements?)>
]]>
<![%final;[
  <!ELEMENT book (title, body, supplements?)>
]]>
```

---
## XML

### Namespaces

Problem: We need to distinguish the same names of elements and attributes in cases when a conflict may occur.
- the application needs to know which elements/attributes it should process
  -  e.g. name of a book vs. name of a company

Idea: expanded name of an element/attribute = ID of a namespace + local name
- The namespace is identified by URI 
- URI is too long → shorter version
  - Namespace declaration = prefix + URI
  - Qualified name (QName) = prefix + local name of an element/attribute 

Note: DTD does not support namespaces (it considers them as any other element/attribute names)
- XML Schema is conversely based on namespaces

#### Examples

- Ex. Namespace
```xml
<pricelist:offer xmlns:pricelist="http://www.eprice.cz/e-pricelist">
  <pricelist:item tax="22%">
    <pricelist:name>
      <bib:book xmlns:bib="http://www.my.org/bib">
        <bib:author>Mark Logue</bib:author>
        <bib:name>The King's Speech: How One Man Saved the British Monarchy</bib:name>
      </bib:book>
    </pricelist:name> 
    <pricelist:price curr="CZK">259</pricelist:price>
  </pricelist:item>
</pricelist:offer>
```

- Ex. Implicit Namespace
```xml
<offer xmlns="http://www.eprice.cz/e-pricelist">
  <item tax="22%">
    <name>
      <bib:book xmlns:bib="http://www.my.org/bib">
        <bib:author>Mark Logue</bib:author>
        <bib:name>The King's Speech: How One Man Saved the British Monarchy</bib:name>
      </bib:book>
    </name>
    <price curr="CZK">259</price>
  </item>
</offer>
```

### Data Model - InfoSet

A well formed XML document → hierarchical tree structure = XML Infoset 
- Abstract data model of XML data


- Information set = the set of information (in the XML document)
- Information item = a node of the XML tree 
  - Types of items: document, element, attribute, string, processing instruction, comment, notation, DTD declaration, … 
  - Properties of items: name, parent, children, content, …
- It is used in other XML technologies
- DTD (in general XML schema) can „modify“ Infoset
  - E.g. default attribute values

![dabf4818.png](attachments/15e42996-b386-4d59-87af-840ece336296/dabf4818.png)

#### Item properties - element

- [namespace name] 
  - (Possibly empty) name of namespace 
- [local name] 
  - Local part of element name 
- [prefix] 
  - (Possibly empty) prefix of namespace 
- [children] 
  - (Possibly empty) sorted list of child items
    - Document order 
      - Elements, processing instructions, unexpanded references to entities, strings and comments 
- [attributes] 
  - (Possibly empty) unsorted set of attributes (Attribute Information Items) 
    - Namespace declarations are not included here 
  - Each item (attribute) is declared or given by the XML schema 
    - Attributes with default value
- [namespace attributes]
  - (Possibly empty) unsorted set of declarations of namespaces 
- [in-scope namespaces] 
  - Unsorted set of namespaces which are valid for the element 
  - It always contains namespace XML 
  - It always contains items of set [namespace attributes] 
- [base URI] 
  - URI of the element 
- [parent] 
  - Document/Element Information Item to whose property [children] the element belongs

#### Post Schema Validation Infoset (PSVI)

- Typed Infoset 
- It results from assigning data types on the basis of validation against an XML schema 
  - We can work directly with typed values 
  - Without PSVI we have only text values 
    - DTD: minimum of data types 
    - XML Schema: int, long, byte, date, time, boolean, positiveInteger, … 
- Usage: in query languages (XQuery, XPath) 
  - E.g. We have functions specific for strings, numbers, dates etc

### Formats

- XML schema = a description of allowed structure of XML data = XML format 
  - DTD, XML Schema, Schematron, RELAX NG, … 
- Standard XML format = a particular XML schema which became standard for a particular (set of) application(s) 
  - Input/output data must conform to the format 
  - Usually it is an acknowledged standard

Categorization:
- Publication of data on the Web
  - XHTML, MathML, SVG, XForms 
- Office SW 
  - Office Open, OpenDocument 
- Technical documentation 
  - DocBook 
- Data exchange in communities 
  - UBL, OpenTravel 
- Web services 
  - SOAP, WSDL, UDDI 
- And many other

#### XHTML

- Results from HTML
  - XHTML 1.0 corresponds HTML 4.01 
    - Adapts HTML so that it corresponds to XML standard 
      - Well-formedness 
- XHTML document must: 
  1. Be valid against one of three XHTML DTDs which are explicitly referenced using DOCTYPE declaration 
  2. Have root element html 
  3. Declare namespace of HTML: http://www.w3.org/1999/xhtml

##### XHTML vs. HTML

- The documents must be well-formed
  - End tags are required
  - Empty elements must have the end tag or the abbreviated version
  - Element tags must be properly nested
  - Attributes must have a value and it must be in quotations
- Names of elements and attributes must be in lower case
- Elements script and style have the #PCDATA type
  - Usage of special characters must correspond to XML 

```xhtml
<p>here is an emphasized <em>paragraph</em>.</p>
<dl compact="compact">
<br/><hr/>
<td rowspan="3">

```
VS
```html
<p>here is an emphasized <em>paragraph.</p></em>
<dl compact>
<br><hr>
<td rowspan=3>
```

##### XHTML DTD

- XHTML 1.0 Strict
  - „Purely“ structural tags of HTML 4.01 + cascading style sheets
- XHTML 1.0 Transitional
  - Constructs from older HTML versions, including those for presentation
    - font, b, i, … 
- XHTML 1.0 Frameset
  - Exploitation of frames

#### SVG

~ Scalable Vector Graphics

- http://www.w3.org/Graphics/SVG/
- 2-dimensional vector graphics 
- Types of graphical objects: 
  - Vector – 2D images, splines 
  - Raster images 
  - Text objects 
- Grouping, formatting, transformations, animation, filtering, … 
- Support: FireFox, Opera, MS IE (plug-in) 
- Tools: inkscape (e.g.)

```xml
<?xml version="1.0"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
          "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">

<svg xmlns="http://www.w3.org/2000/svg" width="467" height="462">
  <rect x="80" y="60" width="250" height="250" rx="20" 
        style="fill:#ff0000;stroke:#000000;stroke-width:2px;"/>
  <rect x="140" y="120" width="250" height="250" rx="40" 
        style="fill:#0000ff;stroke:#000000;stroke-width:2px;
        fill-opacity:0.7;"/>
</svg>
```

#### XForms

- http://www.w3.org/TR/xforms11/
- Description of user interface for XML data – Web forms
- New generation of HTML forms 
- XForms Controls 
  - Which items of interface should be used
  - Visualization is determined by a particular browser
- Parts:
  - Data model – what data are processed
  - User interface – input/output controls and their features

#### OOXML 

~ Office Open XML

- Microsoft
  - For the first time in Office 2007
- Parts of specification:
  - Description of structure of files
    - In general a zip file with XML + other data
  - XML for text editors (file extension docx)
  - XML for spreadsheets (xlsx)
  - XML for presentations (pptx)
  - XML for graphics – DrawingML
  - XML for special items – MS MathML
    - e.g. equations


#### WSDL

- http://www.w3.org/TR/wsdl
- Description of Web Service interface
  - Which operations it offers
  - What is the input and output of each operation
- Example:
  - A Web Service offers an operation make-an-order
  - It accepts customer ID + details
    - order-request
  - It tries to add the requirement to the system
  - It successful, it returns the number of order
    - order-response

##### Example

ARES – Administrativní registr ekonomických subjektů
- http://wwwinfo.mfcr.cz/ares/xml_doc/schemas/index.html

###### Parameters of Operations

```xml
<?xml version="1.0" encoding="UTF-8"?>
<description xmlns="http://www.w3.org/ns/wsdl"
             targetNamespace="http://www.example.cz/ws/orders"
             xmlns:objsrv="http://www.example.cz/ws/orders"   
             xmlns:objsch="http://www.example.cz/schema/orders"   
             xmlns:wsoap="http://www.w3.org/ns/wsdl/soap"   
             xmlns:soap="http://www.w3.org/2003/05/soap-envelope">
  <types>
    <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
               targetNamespace="http://www.example.cz/schema/orders"  
               xmlns="http://www.example.cz/schema/orders">    
      <xs:element name="order-request" type="OrderRequirement"/>     
      <xs:element name="order-response" type="OrderResponse"/>  
      ...
    </xs:schema>
  </types>
  ...
</description>
```

###### Operation

```xml
<?xml version="1.0" encoding="UTF-8"?>
<description>
  <types>
    ... 
  </types>

  <interface name="interface-orders">
    <operation name="make-an-order"               
               pattern="http://www.w3.org/ns/wsdl/in-out">
      <input element="objsch:order-request" />
      <output element="objsch:order-response" />
    </operation>
  </interface>
  ...
</description>
```

###### Way of Communication

```xml
<?xml version="1.0" encoding="UTF-8"?>
<description>
  <types> ... </types> 
  <interface> ... </interface>

  <binding name="SOAP-making-orders"           
           interface="objsrv:interface-orders" 
           type="http://www.w3.org/ns/wsdl/soap"
           wsoap:protocol="http://www.w3.org/2003/05/soap/bindings/HTTP/">    
    <operation ref="tns:make-an-order"       
               wsoap:mep="http://www.w3.org/2003/05/soap/mep/request-response"/>
  </binding>
  ...
</description>
```

###### Service Definition

```xml
<?xml version="1.0" encoding="UTF-8"?>
<description>
  <types> ... </types>
  <interface> ... </interface> 
  <binding> ... </binding>

  <service name="service-making-orders"
           interface="objsrv:interface-orders">
    <endpoint name="endpoint-making-orders"
              binding="objsrv:SOAP-making-orders"
              address="http://www.example.cz/services/making-orders"/>
  </service>
</description>
```

---
## XPath

- Aims: querying, views, transformations, actualization, …
- The development stabilized in W3C in languages XSLT, XPath, XQuery
- XSLT is a language for data transformation
  - Exploits XPath for targeting parts of XML document
  - Has XML syntax 
- XQuery is more suitable for querying – user-oriented
  - Exploits XPath for targeting parts of XML document
- Today: XPath 1.0
  - Note: XPath 2.0 subset of XQuery

XPath is
- Basic language for querying XML data
  - Selecting parts of XML documents
- The idea resembles navigation in a file system 
- XPath does not have XML syntax
- XPath is exploited in multiple other XML technologies 
  - XSLT, XQuery, XML Schema, XPointer, …

###### Example

```xml
<?xml version="1.0"?>
<!DOCTYPE order SYSTEM "order.dtd">
<order date="10/10/2008" status="confirmed">
  <customer number="C992">Steve J.</customer>
  <items>
    <item code="48282811">
      <amount>5</amount>
      <price>22</price>
    </item>
    <item code="929118813">
      <amount>1</amount>
      <price>91934</price>
      <color>blue</color>
    </item>
  </items>
</order>
```
... translates to ...
![4c3fcf1f.png](attachments/15e42996-b386-4d59-87af-840ece336296/4c3fcf1f.png)

### Expression

- XPath expression is a path
- Path consists of steps
  - Absolute path:
    - /Step1/Step2/…/StepN
  - Relative path:
    - Step1/Step2/…/StepN
    - itself cannot be evaluated separately
      - It does not make sense, because we do not know where to start
      - The input must involve both the relative path and one or more nodes of XML documents where the evaluation should start
        - so-called context set

#### Evaluation

- Let P be an XPath path 
- Let C be the context set of nodes for evaluation of P 
  - If P is absolute, then C contains the root node of the document 
  - If P is relative, then C must be specified explicitly 
- If P is empty, then the result of evaluation corresponds to C 
- Else, P is evaluated with regards to C as follows: 
  - Let S be its first step and P’ is the rest of the path, i.e. P = S/P’ 
  - C’ = {} 
  - For each node u from C evaluate S and add the result to C’ 
  - Evaluate P’ with regard to C’

#### Formally

XPath step is formally the following expression:
- `axis::node-test predicate1 ... predicateN`

Axis, node test and list of predicates 
- Predicates are optional

So far we saw only node tests
- The list of predicates was empty
- Axes were abbreviate

##### Axes

Axis specifies the relation of nodes selected in this step with regard to each node `u` from context set `C`
- `child`
- Selected nodes are child nodes of node `u`
- Most common axis

`/order/customer <--> /child::order/child::customer`

There are these axis (they work as expected from their name):
- `child`
- `self`
- `parent`
- `ancestor`
  - All nodes on the path from u to root node
- `ancestor-or-self`
- `descendant`
  - All descendant nodes of node u
- `descendant-or-self`
- `preceding-sibling`
  - ![648c286b.png](attachments/15e42996-b386-4d59-87af-840ece336296/648c286b.png)
  - All siblings of u which precede it in tree traversal
- `preceding`
  - All nodes which precede u in tree traversal EXCEPT for ancestor
- `following-sibling`
  - All siblings of u which follow it in tree traversal
- `following`
  - All nodes which follow u in tree traversal ◼ Except for ancestors
- `attribute`
- `namespace`

![34a81c55.png](attachments/15e42996-b386-4d59-87af-840ece336296/34a81c55.png)

##### Node-test

- Tests nodes selected by the axis
  - Node type, node name

`axis::* predicate1 ... predicateN`:
- All nodes selected by the axis which have a name
  - Name can have an element or an attribute
  - Note: there exists no axis that enables to selected elements and attributes at the same time


`axis::name predicate1 ... predicateN`
 - All nodes with the specified name

###### Examples
```
axis::comment()
axis::processing-instruction()
axis::processing-instruction(“php”
```

For the most commonly used axes and node tests:
```
…/… <=> …/child::… 
…/@… <=> …/attribute::… 
…/.… <=> …/self::node()… 
…/..… <=> …/parent::node()…
…//… <=> …/descendant-or-self::node()/…
```

##### Predicates

- A predicate enables to specify advanced conditions for nodes which were selected by the axis and node test
  - For context node u we find all nodes selected by the axis from node u
  - On input we put those which satisfy node test and all predicates
- Predicate condition can be a relative XPath path P 
  - For node u it returns true if the set of nodes returned by path P from u is non-empty
- Predicated condition can be an absolute XPath path P
  - It returns true if the set of nodes returned by path P is non-empt
- The condition can involve comparison of two operands
  - Operands are XPath expressions
    - XPath path, value, ...
  - Operators are = != < > <= >=
  - String value of node
    - Attribute – normalized value
    - Element – concatenation of text nodes in its subtree
- Operators = != ... 
  - Operands are sets of values/nodes
  - Evaluated as true if there exists a value/node in the left operand and a value/node in the right operand for which the operator evaluates as true
  - Consequences:
    - Expression with = and != can return the same result!
    - x="foo" is not the same as not(x!="foo")
      - There exists a node in x with string value foo
      - All nodes in x have string value foo


###### Examples

`//order[@status]//item`
![f36ea903.png](attachments/15e42996-b386-4d59-87af-840ece336296/c4f08d7e.png)

Difference in = and != and not(=)
![a8d5961d.png](attachments/15e42996-b386-4d59-87af-840ece336296/a8d5961d.png)




##### Built-in Functions

Testing of position :
- Each node in a context set has a position
- Determined by its position in document and the (direction of a) particular path
  - position()
    - indexer
    - Returns the position of node in a context set 
  - last()
    - Returns the number of nodes in a context set


- count()


- +, -, \*, div, mod
- name(), id() 
- concat(), starts-with(), contains(), substring-after(), substring-before(), substring(), ...
- sum(), floor(), ceiling(), ...


### Advanced XPath

XPath 2.0
- Adds a huge number of new functions
  - see http://www.w3.org/TR/xpath-functions/ 
  - Prefixed with fn:
    - Namespace http://www.w3.org/2005/xpath-functions
- Works with ordered collections
  - Adds new constructs
  - Iterations of sequences (for loop), merging of sequences (union, intersect, except), conditions (if-then-else), quantifiers (some/every)
- Relation to XML Schema
  - Nodes are assigned with data types in the sense of XML Schema language
- Backward compatibility with XPath 1.0
  - Expressions from 1.0 return the same value
  - Few exceptions

### Data Types

XPath 1.0
- node-set, Boolean, number, string

XPath 2.0
- sequence, XML Schema data types

#### XPath 2.0 – Data Model

##### Sequence

- sequence is an ordered collection of items
  - The result of an XPath 2.0 expression is a sequence
  - item is either an atomic value or a node
    - atomic value = a value of any simple data type of XML Schema
    - node = an instance of any type of node
      - attribute, element, text, …
      - Node has
      - Identity
      - Data type 
        - XML Schema simple/complex data type
      - Typed value
        - Value according to a data type 
          - Returned by fn:data()
      - String value
        - Type value converted to string (xs:string) 
          - Returned by fn:string()
  - Constructor ()
    - `(1, 2, 3, 4)`
  - Constructor to
    - `(1, 5 to 8)` = `(1, 5, 6, 7, 8)`
  - Constructor can contain XPath expressions
    - `(//book, //cd)`
  - Constructor can be used as a step in an XPath expression
    - `(1 to 100)[. mod 5 = 0]`
    - `//item/(price,value) `
    - `orders[fn:position() = (5 to 9)]`
  - Everything is a sequence
    - `1` = `(1)`
  - Sequences are shallow = do not contain subsequences
    - `(1, (2, 3), 4)` = `(1, 2, 3, 4)`
  - Sequences can contain duplicities
    - `(1, 2, 1 to 2` = `(1, 2, 1, 2)`
  - Sequences can contain atomic values and nodes together
    - `(1, 2, //book)`

##### Iteration

Construct for for iteration of sequences within expressions

```
for $varname1 in expression1,
    ...,
    $varnameN in expressionN
return expression
```

```
fn:sum( for
            $item in //item
        return
            $item/amount * $item/price) 
```

##### Quantifiers

`some/every $variable in expression satisfies test_expression`

If the quantifier is some (every), the expression is true if at least one (every) evaluation of the test expression has the value true; otherwise the expression is false

`every $part in /parts/part satisfies $part/@discounted`

##### Merging

- Union of sequences
  - union or | (| is already in XPath 1.0)
- Intersection of sequences
  - intersect
- Exception of sequences
  - except

- Only for sequences of nodes
  - If the sequence includes an item which is not - error 
- All operators eliminate duplicities
  - Two nodes are duplicate if they have the same identity

```
expression1 union expression2

expression1 intersect expression2

expression1 except expression2
```

```
for
    $item in /order//item
return
    $item/* except $item/pric
```

##### Comparison

- We already know operators for sets
  - =, !=, … 
- New type: comparison of nodes
  - expression1 is expression2
    - true, if both the operands evaluate to the same node
  - expression1 << expression2, resp. expression1 >> expression2
    - true, if the node on the left precedes/succeeds the node on the right (in the document order)
  - If any of the operands is converted to an empty sequence, the result is an empty sequence
  - If any of the operands is converted to a sequence longer than 1, error

- New type: comparison of values
  - lt, gt, le, ge, eq, ne meaning "less than", "greater than", "less or equal", "greater or equal", "equal", "non equal"
  - If any of the operands is converted to an empty sequence, the result is an empty sequence 
  - If any of the operands is converted to a sequence longer than 1, error


##### Conditions

```
if (expression1)
  then (expression2)
  else (expression3)
```

```
for $product in /catalogue//product
return
  if ($product/discount = "yes")
    then $product/discount-price 
    else $product/full-price
```

---
## SQL/XML

- Extension of SQL which enables to work with XML data
  - New built-in data type: XML
  - Publication of relational data in XML
  - XML data querying
- Node: SQL/XML ≠ SQLXM
  - Technology of Microsoft used in SQL Server
  - Not a standard from the SQL family
- Key aspect: XML value
  - Intuitively: XML element or a set of XML elements

### Functions for Data Publication

SQL expressions → XML values
- XMLELEMENT – creating XML elements
- XMLATTRIBUTES – creating XML attributes
- XMLFOREST – creating XML elements for particular tuples
- XMLCONCAT – from a list of expressions creates a single XML value
- XMLAGG – XML aggregatio

#### XMLELEMENT

![1b1c9bac.png](attachments/15e42996-b386-4d59-87af-840ece336296/1b1c9bac.png)

Supports subelements; mixed content; subqueries

##### Namespaces

![98215294.png](attachments/15e42996-b386-4d59-87af-840ece336296/98215294.png)

#### XMLATTRIBUTES

![90f58f66.png](attachments/15e42996-b386-4d59-87af-840ece336296/90f58f66.png)

#### XMLFOREST

- Constructs a sequence of XML elements for
  - Optional declaration of namespaces
  - List of named expressions as arguments 
- If any of the expressions returns NULL, it is ignored 
- If all the expressions return NULL, the result is XML value NULL 
- Each element in the result can be named implicitly or explicitly

![4b232a64.png](attachments/15e42996-b386-4d59-87af-840ece336296/4b232a64.png)

#### XMLCONCAT

![9f4b8ba4.png](attachments/15e42996-b386-4d59-87af-840ece336296/9f4b8ba4.png)

#### XMLAGG

- XMLAGG is an aggregation function
  - Similar to SUM, AVG from SQL
- The argument for XMLAGG must be an XML expression
- For each row in a group G, we evaluate the expression and the resulting XML values are concatenated so that they form a single XML value for the whole group G 
- For sorting we can use clause ORDER BY 
- All NULL values are ignored for the concatenation 
  - If all the concatenated values are NULL or the group is empty, the result is NULL

![b053637c.png](attachments/15e42996-b386-4d59-87af-840ece336296/b053637c.png)

#### XML Data Type and Querying

- XML data type can be used anywhere, where SQL data types (e.g. NUMBER, VARCHAR, …)
  - Type of column, parameter of a function, SQL variable, …
  - Its value is an XML value
- XML Infoset modification: XML value is
  - XML element
  - Set of XML elements
  - → Not each XML value is an XML document
- Querying:
  - XMLQUERY – XQuery query, results are XML values
  - XMLTABLE – XQuery query, results are relations
  - XMLEXISTS – testing of conditions

##### XMLQUERY

![0e52980e.png](attachments/15e42996-b386-4d59-87af-840ece336296/0e52980e.png)

##### XMLTABLE

![d1bcec72.png](attachments/15e42996-b386-4d59-87af-840ece336296/d1bcec72.png)

##### XMLEXISTS

![b9b2f3ef.png](attachments/15e42996-b386-4d59-87af-840ece336296/b9b2f3ef.png)

##### Other Constructs

- XMLPARSE 
  - transforms the given SQL string value to XML value
- XMLSERIALIZE
  - transforms the given XML value to SQL string value
- IS DOCUMENT
  - tests whether the given XML value has a single root element
- …

---
## XML Schema

- XML schema = allowed structure of XML data in any available language 
  - DTD, XML Schema, RELAX NG, Schematron, …
- XML Schema = one of the languages 
  - „XML schema in XML Schema" 
  - XSD = XML Schema definition 
    - Counterpart for DTD 
- There are other options: XML-schema, XMLSchema, XML-schema, Xschema, … 
  - In Czech: much more options

### Advantages

- Does not require special syntax
  - XSDs = XML documents
- Strong support for data types
  - A set of built-in data types (e.g. Boolean, date, ...)
  - User-defined data types
- We can (easily) express number of occurrences of elements

```dtd
<!ELEMENT person (name, e-mail, e-mail?, e-mail?, e-mail?, e-mail?, relations?)>
```

- We can define elements with the same name but different content
  - In DTD we can not – all elements are defined at the same level
- We can define empty elements and elements which can be specified without content
- We can specify the exact structure of mixedcontent elements
- We can (easily) express unordered sequences

```dtd
<!ELEMENT name ((first, surname)|(surname, first))>
```

- We can re-use various parts of the schema
  - Data types, sets of elements, sets of attributes, …
  - Object-oriented features
- Keys and references
  - Specification of context
  - Combination of elements and/or attributes
- We can define the same thing in various ways
- Preserves DTD structures
  - Except for entities

### Disadvantages = advantages

- Does not require special syntax
  - XSDs are long and less lucid than DTDs
  - More complex schemas are difficult to understand
  - Complex description
    - Elements and attributes are defined using elements and attributes
- We can define the same thing in various ways
  - An advantage for the user
  - A disadvantage for processing

Versions:
- 1.0
- 1.1

### Basics

- XSD = a well-formed XML document
  - XML declaration, root element, …
  - Validity against XSD of XML Schema language

- Components of the language = elements
  - Features – subelements/attributes
  - Defined in XML Schema namespace

```xml
<?xml version="1.0" encoding="windows-1250"?>
<schema ...>  
  ... <!-- XML schema definition --> ... 
</schema>
```

###### Examples

```xml
<?xml version="1.0" encoding="windows-1250"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"> 
    ... <!-- XML schema definition  --> ...
</xs:schema>
```

```xml
<?xml version="1.0" encoding="windows-1250"?>
<root_element_of_XML_document
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
    xsi:noNamespaceSchemaLocation="schema2.xsd">
    ... <!-- XML document --> ...
</root_element_of_XML_document
```

- XML Schema namespace
- Namespace of XML Schema instances = XML documents valid against an XSD
- URL of XSD file

#### Root-element

- Any globally defined element can be the root element of an XML document
  - Globally defined = direct subelement of element schema
    - must have a name >> can be reused
- Globally defined components have in XML Schema special behaviour
  - Elements, attributes, data types, sets of elements, sets of attributes
    - Elements → root elements, ...
    - Other → can be used repeatedly, ... (see later)

#### How to work

- XML schema definition:
  - Definition of data types
  - Definition of elements and attributes 
    - Name + data type 
- Components of the language: 
  - Basic – simple data type, complex data type, element, attribute, set of elements, set of attributes 
  - Advanced – identity restriction, substitution groups, wildcards, external schemas, … 
- „Kit" – we build complex components from simpler ones 
  - We can extend, restrict, refer, …

### Basic components

#### Simple Data Types

- For when Element doesnt contain subelements or attributes
- Element content or attribute value is always textual
- Simple data type = restriction of textual values to a particular subset 
- Types:
  - Built-in – pre-defined
    - see Specification Part 2: Datatypes
  - User-defined – specified by the user
    - Derived from other data types

![adac0136.png](attachments/15e42996-b386-4d59-87af-840ece336296/c47f99c3.png)

![a7d62d93.png](attachments/15e42996-b386-4d59-87af-840ece336296/a7d62d93.png)

##### Primitive Data Types

![163759ab.png](attachments/15e42996-b386-4d59-87af-840ece336296/163759ab.png)
![aba36089.png](attachments/15e42996-b386-4d59-87af-840ece336296/c66e6bd8.png)

##### User defined simple types

- Enables to define own data types
- Attributes:
  - name – (optional) name of the data type
  - final – forbids further derivation
    - restriction, union, list, #all
- Derived from another (built-in / user-defined) data type via
  - restriction
  - union
  - list

```xml
<xs:simpleType name="Ports">
  <xs:restriction base="xs:integer">
    <xs:enumeration value="111"/>
    <xs:enumeration value="21"/>
    <xs:enumeration value="80"/> 
  </xs:restriction>
</xs:simpleType>

<xs:simpleType name="NonEmptyString" final="#all">
  <xs:restriction base="xs:string">
    <xs:minLength value="1"/>
    <xs:maxLength value="10"/>
  </xs:restriction>
</xs:simpleType> 
```

```xml
<xs:element name="PortNumber" type="Ports"/>
<xs:element name="ServerName" type="NonEmptyString"/>
```

###### Restriction

- take a "base type" and restrict it's values


- length – the number of items of a particular data type
- minLength – the minimum number of items of a particular data type
- maxLength – the maximum number of items of a particular data type 
- pattern – regular expression describing items of the data type
  - Operators: `. (any character) \ (escape or meta character) ? * + | () (group) {} (repetition) [] (interval), \s (white space) \S (non white space) \d (number) \n \t`
  - Example. `\*\d*\* … "*1234*", a{2,4} … "aaa", (\d|[A-Z])+ … "3", "U2"`
- enumeration – a set of values
- maxInclusive – values <= specified value
- minInclusive – values >= specified value
- maxExclusive – values < specified value 
- minExclusive – values > specified value
- totalDigits – maximum number of digits
- fractionDigits – maximum number of fraction digits
- whiteSpace – processing of whitespaces 
  - preserve – no changes
  - replace – characters CR, LF and tabulator are replaced with a space 
  - cllapse – in addition, all leading and trailing whitespaces are removed and sequences of whitespaces are replaced with a single one

```xml
<xs:simpleType name="nameWithCapitalLetters">
  <xs:restriction base="xs:string">
    <xs:whiteSpace value="collapse"/>
    <xs:pattern value="([A-Z]([a-z])* ?)+"/>
  </xs:restriction>
</xs:simpleType>
```

###### List

- Creates a list of values of the original data type delimited using whitespaces
  - Problem: list of strings vs. white space delimiters
- Attributes:
  - itemType – original type
    - Or specified using subelement simpleType
- Multivalue data types
  - We cannot derive from other multivalue data types
    - i.e. create a list of lists
  - NMTOKENS, IDREFS, ENTITIES

```xml
<xs:simpleType name="ListOfFloats">
  <xs:list itemType="xs:float"/>
</xs:simpleType>

<xs:element name="Temperatures"
            type="ListOfFloats"/>
            
            
            
<Temperatures>11 12.5 10.2</Temperatures>
<Temperatures>-3.14 0 -1.5</Temperatures>

```

###### Union

- unite the values of all named types


- Creates a union of values of original data types
- Attributes:
  - memberTypes – original data types
    - Or specified using subelements simpleType

```xml
<xs:simpleType name="NonZeroIntegers">
  <xs:union memberTypes="xs:positiveInteger xs:negativeInteger"/>
</xs:simpleType> 

<xs:element name="Temperature" type="NonZeroIntegers"/>



<Temperature>11</Temperature>
<Temperature>-3</Temperature>
<Temperature>10</Temperature>
```

##### Global VS Local definition

local/anonymized type - use when it is a technical detail

```xml
<xs:simpleType name="TypeZeroTo100">
  <xs:union>
    <xs:simpleType>
      <xs:restriction base="xs:positiveInteger">
        <xs:minInclusive value="1"/>
        <xs:maxInclusive value="100"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType>
      <xs:restriction base="xs:string">
        <xs:enumeration value="zero"/> 
      </xs:restriction>
    </xs:simpleType> 
  </xs:union>
</xs:simpleType>
```

#### Attributes

- Name + simple data type
- Built-in – value of attribute type
- Globally defined – value of attribute type
- Locally defined – subelement simpleType

Attributes:
- default – implicit value
- fixed – fixed value
- use – occurrence
  - optional,
  - required
  - prohibited (specified value cannot be used)
Attributes can be also defined globally / locally
- In both the cases it has a name
- Globally – element attribute is a subelement of element schema
  - We can refer to it using references
- Locally – within a definition of a complex type or a set of attributes
  - Just local usage 

###### Examples

```xml
<xs:attribute name="Age" type="xs:positiveInteger"/> 

<xs:attribute name="Name" type="NonEmptyString"/> 

<xs:attribute name="PhoneNumber">
  <xs:simpleType>
    <xs:restriction base="xs:string">
      <xs:pattern value="\d{3}-\d{6}"/>
    </xs:restriction>
  </xs:simpleType>
</xs:attribute>


<person Age="30"
        Name="H. Simpson"
        PhoneNumber="123-445566"/>
```

#### Elements

- Name + simple / complex data type
  - Simple type – element without attributes with a text content
    - Built-in – value of attribute type 
    - Globally defined – value of attribute type
    - Locally defined – subelement simpleType
  - Complex type – other types of elements 
    - Globally defined – value of attribute type 
    - Locally defined – subelement complexType
- Enables to define keys/references
  - unique, key, keyref – see later

```xml
<xs:element name="name" type="xs:string"/> 

<xs:element name="surname">
  <xs:simpleType>
    <xs:restriction base="xs:string">
      <xs:minLength value="2"/>
    </xs:restriction>
  </xs:simpleType>
</xs:element>


<name>Marge</name>
<surname>Simpson</surname>
```

Attributes:
- nillable – possible empty content
- default – implicit value
  - Only for elements with text content
- fixed – fixed value
  - Only for elements with text content
Elements can be also defined globally / locally
- In both the cases it has a name
- Globally – element element is a subelement of element schema
- We can refer to it using references
  - Root elements of XML documents
- Locally – within a definition of a complex type
  - Just local usage ?#!

#### Complex Data Types

- For definition of more complex types of elements
  - Relations element-subelement and element-attribute
  - Numbers and order of subelements
  - Since version 1.1: Conditions for values of subelements/attributes
    - Using XPath
    - assert – see later
- Consists of:
  - Specification of content
    - empty = an empty element
  - Specification of attributes 
    - empty = an elements without attributes

```xml
<xs:complexType name="TypeAddress">

  <!-- specification of content -->
  <xs:sequence>
    <xs:element name="Street" type="xs:string"/>
    <xs:element name="Number" type="xs:integer"/>
    <xs:element name="City" type="xs:string"/>
  </xs:sequence>
  
  <!-- specification of attributes -->
  <xs:attribute name="Country" type="xs:NMTOKEN" default="CZ"/>
</xs:complexType> 


<xs:element name="Address" type="TypeAddress"/>


<Address Country="SK">
  <Street>Red Street</Street>
  <Number>6</Number>
  <City>Bratislava 16</City>
</Address>
```

Attributes:
- mixed – an element with mixed content 


Can be also defined globally / locally 
- Usage same as in case of simple types 


Types of content: 
1. with a simple textual content (simpleContent) 
2. sequence of components (sequence) 
3. choice of components (choice) 
4. unordered sequence of elements (all) 
5. model group (group) 
6. with a complex content (complexContent)

In reality only 1. and 6. - 2. to 5. are variants of 6.

##### Simple content

- no subelements

- The content of element is a simple type + attributes

Derivation:
- extension – adding attributes
```xml
<xs:complexType name="Type">
  <xs:simpleContent>
    <xs:extension base="xs:string">
      <xs:attribute name="Subtype" type="xs:string"/> 
    </xs:extension>
  </xs:simpleContent>
</xs:complexType>

```
- restriction – adding attributes + type restriction
```xml
<xs:complexType name="CarType">
  <xs:simpleContent>
    <xs:restriction base="xs:string">
      <xs:enumeration value="Audi"/>
      <xs:enumeration value="VW"/>
      <xs:enumeration value="BMW"/>
      
      <xs:attribute name="Subtype" type="xs:string"/>
    </xs:restriction> 
  </xs:simpleContent>
</xs:complexType>

```


##### Sequence

- The content is formed by all the specified items in the given order

- maxOccurs
  - default = 1
- minOccurs
  - default = 1

```xml
<xs:complexType name="person">
  <xs:sequence>
    <xs:element name="name"    type="xs:string"
                               maxOccurs="5"/>
    <xs:element name="surname" type="xs:string"/>
    <xs:element name="born" type="xs:date"/>
    <xs:element name="note" type="xs:string"
                            minOccurs="0"/>
  </xs:sequence>

  <xs:attribute name="Id" type="xs:ID"/> 
</xs:complexType>
```

##### Choice

- choose exactly one

```xml
<xs:complexType name="TypePriceList">
  <xs:choice>
    <xs:sequence>
      <xs:element name="BasicPrice" type="xs:string"/>
      <xs:element name="FullPrice" type="xs:string"/>
    </xs:sequence>
    <xs:element name="BargainPrice" type="xs:string"/>
    <xs:element name="StudentPrice" type="xs:string"/>
  </xs:choice>
</xs:complexType>
```

##### All

- The content is formed by the specified elements in an arbitrary order

```xml
<xs:complexType name="TypeBook">
  <xs:all>
    <xs:element name="Name"   type="xs:string"/>
    <xs:element name="Author" type="xs:string"/>
    <xs:element name="ISBN"   type="xs:string" minOccurs="0"/>
  </xs:all>
</xs:complexType>
```

###### Version differences

Version 1.0: maxOccurs of elements and the whole set is <= 1
- What if we want maxOccurs > 1?
  - Idea: Combination of choice and maxOccurs > 1
  - Can lead to a non-deterministic data model
    - Not allowed by specification, but there can exist a parser which supports it
Version 1.1: maxOccurs of elements > 1
- In general, not everything is allowed, but the rules are not so strict
- has to be deterministic

##### Group

- Contains a sequence / choice / set of items (elements)
- Always declared globally and has a name
  - Repeating usage of the content using references

References in general:
- We declare them using the same construct as the referenced item
  - Instead of attribute name we use attribute ref
- The same principle can be used for model groups, elements, attributes and groups of attributes (see later)
  - In case of elements and attributes only the globally defined ones can be referenced


```xml
<xs:group name="CommonElements">
  <xs:sequence>
    <xs:element name="Name" type="xs:string"/>
    <xs:element name="Author" type="xs:string"/>
    <xs:element name="Date" type="xs:date"/>
  </xs:sequence>
</xs:group>

<xs:complexType name="TypeBook">
  <xs:sequence>
    <xs:group ref="CommonElements" minOccurs="0"/>
    <xs:element name="ISBN"  type="xs:string"/>
    <xs:element name="Publisher" type="xs:string"/>
  </xs:sequence>
</xs:complexType>
```

###### References

```xml
<xs:element name="Name">
  <xs:simpleType>
    <xs:restriction base="xs:string">
      <xs:minLength value="1"/>
    </xs:restriction>
  </xs:simpleType>
</xs:element>


<xs:complexType name="Book">
  <xs:sequence>
    <xs:element ref="Name"/>
    <xs:element name="Publisher" type="xs:string"/>
  </xs:sequence>
</xs:complexType>
```

##### Complex content

- we want subelements and attributes


- Inference of new types from already existing ones
- restriction – the new type is a subset of the original one
  - Restriction of occurrences of an element/attribute
    - Removing of elements: maxOccurs="0"
    - Removing of attributes: use="prohibited"
  - Restriction of allowed values of simple types (of attribute values, content of text elements)
- extension – the new type contains original and new data (in this order)
  - A kind of inheritance

```xml
<xs:complexType name="Publication">
  <xs:sequence> 
    <xs:element name="Name"   type="xs:string"/>
    <xs:element name="Author" type="xs:string"
                              maxOccurs="unbounded"/>
    <xs:element name="Published" type="xs:gYear"/>
  </xs:sequence>
</xs:complexType>


<!-- restriction -->
<xs:complexType name="PublicationWithOneAuthor">
  <xs:complexContent>
    <xs:restriction base="Publication">
      <xs:sequence>
        <xs:element name="Name" type="xs:string"/>
        <xs:element name="Author" type="xs:string"/>
        <xs:element name="Published" type="xs:gYear"/> 
      </xs:sequence>
    </xs:restriction>
  </xs:complexContent>
</xs:complexType>


<!-- extension -->
<xs:complexType name="TypeBook">
  <xs:complexContent>
    <xs:extension base="Publication">
      <xs:sequence>
        <xs:element name="Publisher" type="xs:string"/>
        <xs:element name="ISBN"      type="xs:string"/>
      </xs:sequence>
    </xs:extension>
  </xs:complexContent>
</xs:complexType>
```

Related attributes of complexType:
- abstract – abstract data type
  - Cannot be assigned to any element
  - First we must derive a new type
- final – forbids further derivation
  - Values: restriction, extension, #all
  - Like with simple types


#### Invariants - assert

- Version 1.1: Possibility of specification of conditions for existence or values of subelements / attributes
  - Using XPath
- Similar to CHECK constraint in databases
- Attributes:
  - test – XPath expression which must hold true
- Meaning:
  - assert – error, when the expression does not return true

```xml
<xs:complexType name="Interval">
  <xs:attribute name="min" type="xs:integer"/> 
  <xs:attribute name="max" type="xs:integer"/>
  <xs:assert test="@min < @max"/>
</xs:complexType>
```

#### Attritbute group

- Contains a set / group of attributes
  - Similar to a model group of elements
- Always declared globally and always has a name
  - Repeating usage of a set of attributes
    - Using references
    - The same principle can be used for globally defined attributes

```xml
<xs:attributeGroup name="CommonAttributes">
  <xs:attribute name="Borrowed" type="xs:boolean"/>
  <xs:attribute name="Id"       type="xs:ID"/>
</xs:attributeGroup>

<xs:complexType name="TypeBook"> 
  <xs:sequence>
    <xs:element name="Name"     type="xs:string"/>
    <xs:element name="Publisher" type="xs:string"/>
  </xs:sequence>
  
  <xs:attributeGroup ref="CommonAttributes"/>
</xs:complexType> 
```

### Advanced components

#### Namespaces

- XML Schema enables to define a namespace 
  - Target namespace
- Parts of a namespace vs. XML Schema constructs:
  - All element partition 
    - Globally defined elements 
  - Per element type partitions 
    - Attributes of elements 
  - Global attribute partition 
    - Globally defined attributes
- Element schema has two special attributes: 
  - elementFormDefault, attributeFormDefault
  - Values: qualified/unqualified 
    - Default: unqualified 
  - Denote the necessity of qualification of element/attribute names with namespace prefixes

 Namespaces: ◼ Namespace of XML Schema language ◼ Target namespace ◼ Implicit namespace  We do not have to use a prefix for defined items ◼ If we do not define a target namespace, this holds implicitly

```xml
<?xml version="1.0" encoding="windows-1250"?>
<xs:schema
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    targetNamespace="http://www.mff.cuni.cz/MySchema" 
    xmlns="http://www.mff.cuni.cz/MySchema"
    elementFormDefault="qualified">
  
  ... <!-- definition of XML schema --> ... 
</xs:schema>
```

#### Include

Including of a schema with the same / none target namespace
- The components of the included schema become parts of the current target name space
- Like if we just copy the content of the schema

#### Import

Import of components with any namespace
- Globally defined items can be then used for definition of the current schema 
  - It is not a copy of the imported schema! 

Attributes:
- schemaLocation – URI of imported schema
- namespace – namespace of imported schema
  - If specified, we use qualified names


### Identity Restriction

- ID, IDREF, IDREFS – taken from DTD 
  - Just for attributes 
  - Must hold within the whole document 
- XSD identity restrictions: 
  - Key – compulsory, not-null, unique value (key) 
  - Unique – not-null, unique value (unique) 
  - Reference to key / unique value (keyref) 
    - Similar to keys and foreign keys in relational databases 
- Based on a (subset of) XPath

Subset of XPath
- Steps = elements / attributes (@)
- Can contain:
  - `.` – current element
  - `/` – child element / attribute
  - `//` – descendant in any depth
  - `*` – any name

Attributes:
- name – name of identity restriction
- refer – reference to an existing identity restriction
  - Just for keyref

Content:
- selector – a set of elements within which the restriction must hold
  - Can be used only once
- field – a set of subelements or attributes (relatively to the set from selector) bearing the restriction
  - At least one
  - Can be a combination of elements / attributes

#### Unique

```xml
<xs:element name="Library">
  ...
  <xs:element name="Book" maxOccurs="unbounded">
    ...
    <xs:element name="ISBN" type="xs:string"
                            minOccurs="0"/>
    ...
  </xs:element>
  
  <xs:unique name="UniqueISBN">
    <xs:selector xpath="./Book"/>
    <xs:field xpath="./ISBN"/>
  </xs:unique>
</xs:element> 
```

#### Key

```xml
<xs:element name="Library">
  ...
  <xs:element name="Book" maxOccurs="unbounded">
    ...
    <xs:element name="ISBN" type="xs:string"/>
    ...
  </xs:element>
  
  <xs:key name="PrimaryKey">
    <xs:selector xpath="./Book"/>
    <xs:field xpath="./ISBN"/>
  </xs:key>
</xs:element>
```

#### Keyref

```xml
<xs:element name="Library">
  <!-- The previously defined element and constraint -->
  <xs:element name="Author" maxOccurs="unbounded">
    ...
    <xs:element name="BestBook">
      ...
      <xs:element name="ISBN" type="xs:string"/>
      ...
    </xs:element>
    ...
  </xs:element>
  
  <xs:keyref name="ForeignKey" refer="PrimaryKey">
    <xs:selector xpath="./Author/BestBook"/>
    <xs:field xpath="./ISBN"/>
  </xs:keyref>
</xs:element> 
```

### Implicit Substitutability (Substitutability of Data Types)

- Implicit = we do not need to specify anything in the schema
- In the document we specify the data type 
  - Derived from the original
- Using attribute xsi:type

- Attribute block of element complexType ◼ Values: restriction, extension, #all

```xml
<Publication>
  <Name>The King's Speech</Name>
  <Author>M. Logue</Author>
</Publication>

<Publication xsi:type="TypeBook">
  <Name>Elizabeth the Queen</Name> 
  <Author>S. B. Smith</Author>
  <ISBN>123-456-789</ISBN>
</Publication>
```

### Substitution Groups (Substitutability of Elements)

- Extension of substitutability 
- Mechanism of explicit allowing / forbidding of substitution of whole elements (i.e., not only their data types)
- Idea: Elements are assigned to a substitution group of a leading element denoted using its name 
  - The leading element can be then substituted with elements in its substitution group
- Conditions:
  - All elements must be defined globally
  - An element in a substitution group must have the same data type as the leading element or a type derived from its data type
  - Relation „to be in a substitution group" is transitive


```xml
<xs:element name="Publication" type="TypePublication"/>
<xs:element name="Book"        type="TypeBook"
            substitutionGroup="Publication"/>
<xs:element name="Journal"     type="TypeJournal" 
            substitutionGroup="Publication"/>
            
<xs:element name="Library">
  <xs:complexType>
    <xs:sequence> 
      <xs:element ref="Publication" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
</xs:element> 
```

The features of the groups are given by attributes of element
- substitutionGroup – name of the leading element, i.e. the group to which the element is assigned 
- abstract – abstract element 
  - The element cannot be used in a document, it must be always substituted with an element from its substitution group 
- final – blocking of adding of elements to the substitution group of the element 
  - Values: extension, restriction, #all 
- block – blocking of substitution of the element (there can be elements in its substitution group, but we cannot substitute for it at the particular position)
  - Values: extension, restriction, #all

### Wildcards

- processContents – the way of validation of the content
  - strict – strict validation
  - lax – validation in case the parser finds the component
  - skip – no validation
- notNamespace – list of namespaces from which we cannot use items
  - ##targetNamespace, ##local
  - Since version 1.1
- notQName – list of elements / attributes we cannot use
  - Since version 1.1

Element any
- Attributes:
  - nameSpace, processContents, notNamespace, notQName – the same meaning
  - minOccurs
  - maxOccurs


### Notation

> Same as DTD.

### Annotation

- Denoted for documentation / comments of the schema
  - XML comments can be used as well
  - Part of any schema component
- Element appinfo – information for a program
  - Attributes:
    - source – URI of an external file, where the information is stored
- Element documentation – information for a human
  - Attributes:
    - source – URI of an external file, where the information is stored
    - xml:lang – language of the information, when provided directly in the schema

---
## XSLT

~ XML Stylesheet Language for Transformations

Originally: transformation of XML documents for the purpose of their visualization
- XSL Formatting Objects (XSL-FO)
- Pages, regions, lines, …

Now:
- A language with (almost) the same expressive power as XQuery
  - XML query language
- Output: any text format

Input:
```xml
<?xml version="1.0"?>
<order number="322" date="10/10/2008" status="dispatched">
  <customer number="C992">
    <name>Martin Nečaský</name>
    <email>martinnec@gmail.com</email>
  </customer>
  <items>
    <item code="48282811">
      <name>CD</name>
      <amount>5</amount>
      <price>22</price>
    </item>
    <item code="929118813">
      <name>Dell Latitude D630</name>
      <amount>1</amount>
      <price>30000</price>
      <colour>blue</colour>
    </item> 
  </items>
</order>
```

Output:
```xml
<?xml version="1.0"?>
<html>
  <head>
    <title>Order no. 322 – Martin Nečaský</title>
  </head>
  <body>
    <table>
      <tr>
        <td>CD</td>
        <td>22 CZK</td>
        <td>5 pc</td>
      </tr>
      <tr>
        <td>Dell Latitude D630</td>
        <td>30000 CZK</td>
        <td>1 pc</td>
      </tr>
    </table>
    <div>Total price: 30110 CZK</div>
    </body>
</html>
```

### Basic Principles

- Input: one or more XML documents
- Output: one or more documents
  - Not only XML
  - In the basic version one 
  - Input data are not modified
- XSLT script = XML document
  - Must follow the XML rules
    - Prologue, well-formedness, validity, …
  - Can be processed using any XML technology
    - DOM, SAX, XPath, XSLT, XQuery…
- Using XSLT we create a transformation script
- The script consists of templates
- A template is applied on a selected node of the input XML document and produces the specified output
  - It can trigger application of other templates on the same node or other nodes 
  - It can read the data from the input document or other documents

### Basics

XSLT uses XML format
- Prologue
- Root element stylesheet

- Root element xsl:stylesheet
  - Namespace of XSLT language
  - Other namespaces (if necessary)
- Attribute version – XSLT version
  - 1.0, 2.0, 3.0

- Element xsl:output
  - Child element of element xsl:stylesheet
  - Denotes the type of output document
    - xml, pdf, text, … 
      - The XSLT parser may add, e.g., prologue
      - Implementation dependent
  - indent = "yes" denotes whether the XSLT parser indents the output
    - Adds formatting white spaces

```xml
<?xml version="1.0" encoding="windows-1250"?>
<stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns="http://www.w3.org/1999/xhtml"
    version="1.0">
    
  <xsl:output method="xml" indent="yes" />
  
  <xsl:template> ... </xsl:template> 
  <xsl:template> ... </xsl:template>
  
  ...
  
</stylesheet>
```

### Templates

Element xsl:template 
- Child element of element xsl:stylesheet
- Describes a single template 
- The script can (and usually does) contain multiple templates 
  - All at the same level

Input: XML node which can be selected using an XPath path
- Element, attribute, text, ...

Output:
- XML fragment (sequence of XML nodes)
- In general any text (HTML, PDF, CSV, …)

Two types:

#### Unnamed

- called on elements found by match
 

- Element xsl:template with attribute match
- The value of the attribute is a sequence of XPath paths delimited with ‘|’
- Steps of XPath paths can use axes child attribute or abbreviation ‘//’

```xml
<xsl:template match="[xpath path ['|' xpath path]*]">
  ...
</xsl:template>
```

#### Named

- called via its name by a caller


- Element xsl:template with attribute name (unique name)
- The value of the attribute is the name of the template

```xml
<xsl:template name="[template name]"> 
  ... 
</xsl:template>
```

#### XSLT script

- is executed using a program called XSLT processor
  - saxon, xsltproc, ...
  - Often also built in browsers
- is executed over an input XML document
  - We can have multiple input documents
  - Others are referenced from the script

- find root element
- use UNNAMED template on it

##### How does it work

- XSLT processor works according to the following algorithm:
  - Create a context set of nodes C and add there the root node of the input XML document 
  - While C is non-empty do:
    - Take the first node u from C
      - The order is given by the order in the XML document
    - Find the most suitable template for u and process it according to the template
      - Which template is the most suitable?
      - What if there is no suitable template?
        - What is the output of an empty XSLT script? The processing might extend C.


- The algorithm for finding the most suitable template for node u:
  - We search among unnamed templates
    - i.e. those with attribute match
  - We consider only those templates, whose XPath path P in attribute match describes (covers) node u
    - i.e. u is from some part of the document accessible using P


- What if there are multiple suitable templates?
  - We can always apply only one
  - We take the one with the highest priority
    - It can be set explicitly using attribute priority of element xsl:template 
    - If it is not set, the priority is evaluated implicitly as follows:
      - 0.5: path with more than one step
      - 0: element/attribute name
      - -0.25: *
      - -0.5: node(), text(), …
      - if more than one >> use document order, take last


- What if there is no suitable template?
  - We have implicit (pre-defined, default) templates → there is always a template to be applied
  - They have the lowest priority
    - i.e. they are applied only if there is no other option
- Consequence: An empty XSLT output applies only implicit templates
  - i.e. an empty XSLT script does something 
    - see later

#### Body

1. Creating elements and/or attributes
    - Directly (writing a text) or using elements `xsl:element` and `xsl:attribute`
2. Creating text nodes 
    - Directly (writing a text) or using element `xsl:text`
3. Access to input data 
    - Using element `xsl:value-of`
4. Calling other templates 
    - Using elements `xsl:apply-templates` and `xsl:call-template`
5. Variables and parameters 
    - Using elements `xsl:variable` and `xsl:param`
6. Repetition
    - Using element `xsl:for-each`
7. Branching
    - Using elements `xsl:if` and `xsl:choose`

##### 1 Creating Elements/Attributes

- In the body of the template we directly write the output
  - Everything that does not belong to the XSLT namespace forms the output

"Pocitany zpusob"
- Or we use element xsl:element
  - Creates an element with the given name and content 
    - Denoted using attribute name and element content
- … and element xsl:attribute
  - Creates an attribute with the given name and value
    - Denoted using attribute name and element content
- Elements xsl:… enable to “calculate" element/attribute name
  - e.g. from input data

```xml
<xsl:template match="/">
  <html>
    <head>
      <title> <!-- creating of the title of the order --> </title>
    </head>
    <table border="1">
      <!-- generating of lines for items of the order -->
    </table>
    <!–- generating of the total price -->
  </html>
</xsl:template>
```

```xml
<xsl:template match="/">
  <orders>
    <xsl:for-each select="//order">
      <order>
        <xsl:if test="./@status">
          <xsl:element name="{./@status}"> YES </xsl:element>
        </xsl:if>
      </order>
    </xsl:for-each>
  </orders>
</xsl:template>
```


##### 2 Text Nodes

- In the body of a template we can directly write text output

```xml
<xsl:template match="/">
  <html> 
    <head>
      <title> Order no. <!–- order number --> – <!–- customer name --> </title>
    </head>
    ...
  </html> 
</xsl:template>
```

- Using xsl:text

```xml
<xsl:template match="/">
  <html>
    <head>
      <title>
        <xsl:text>Order no.</xsl:text>
        <!–- order number -->
        <xsl:text>-</xsl:text>
        <!–- customer name -->
      </title>
    </head>
    ...
  </html>
</xsl:template>
```

##### 3 Input Data

- The access to the input data is enabled by element `xsl:value-of`
  - Attribute select specifies the value
    - Using an XPath path
    - The expression is evaluated in the context of the current node being processed by the template
  - The resulting value forms the output
  - The resulting value is text
    - String value

```xml
<xsl:template match="/">
  <html>
    <head>
      <title>
        <xsl:text>Order no.</xsl:text>
        <xsl:value-of select="order/@number" />
        <xsl:text>-</xsl:text>
        <xsl:value-of select=".//customer/name" />
      </title>
    </head>
    ...
  </html>
</xsl:template>
```


##### 4 Calling Other Templates

Problem: the XSLT parser finds the most suitable template for transformation of root node (usually match="/") of the input XML document
- What next? 
- We want to transform also other nodes in the document tree

###### Element xsl:apply-templates

- for unnamed


- At the place of calling it initiates transformation of other nodes
  - By default child nodes of the currently processed node
- Using attribute select we can specify other nodes than child nodes
  - Using an XPath path
- The selected nodes are processed in the same way as the current node
  - They are added to the context set C
  - The most suitable template is found for each node, …

```xml
<xsl:template match="/">
  <html>
    <head>
      ...
    </head>
    <table>
      <xsl:apply-templates select=".//item"/>
    </table>
    
    <table>
      <xsl:apply-templates /> <!-- implicitly for children-->
    </table>
    ...
  </html>
</xsl:template>
```

###### Element xsl:call-template

- for named


- Application of a particular template on a particular set of nodes
  - The template is specified using its name (attribute name)
- XSLT parser does not look for the most suitable template, but it applies the one with the specified name
  - Similar to calling a function/procedure

```xml
<xsl:template match="item">
  <tr>
    ...
    <td>
      <xsl:call-template name="value-added-tax" /> <!-- current context node passed -->
      <xsl:text> CZK</xsl:text>
    </td> 
  ...
  </tr>
</xsl:template>


<xsl:template name="value-added-tax">
  <xsl:value-of select = "./price * 1.19" />
</xsl:template>
```

##### 5 Variables and Parameters

Variable enables to store a value and refer to it
- Element `xsl:variable` with attribute name and (optional) attribute select
- Local (within templates)
- Global (child nodes of element `xsl:stylesheet`)

Parameter is a variable which is “visible outside” a template
- When calling a template, we can specify also its parameters
- Element `xsl:param` with attribute name and (optional) attribute select

```xml
<xsl:template match="item">
  <tr> 
    ...
    <td>
      <xsl:call-template name="value-added-tax">
        <xsl:with-param name="price" select="./price" />
      </xsl:call-template>
      <xsl:text> CZK</xsl:text>
    </td>
    ...
  </tr>
</xsl:template>


<xsl:template name="value-added-tax">
  <xsl:param name="price" select="0" />
  <xsl:value-of select = "$price * 1.19" />
</xsl:template>
```

Note:
- The values of variables and parameters cannot be changed
  - Once we set the value, we cannot modify it
  - We are in functional programming, not imperative

wont work
```xml
<xsl:variable name="total-price"> 
  <xsl:value-of select="0" /> 
</xsl:variable>


<xsl:template match="/"> 
  ...
  <xsl:apply-tempates select=".//item" />
  ...
  <xsl:text>Total price: </xsl:text>
  <xsl:value-of select="$total-price" /> 
</xsl:template>


<xsl:template match="item">
  <tr>
    ...
  </tr>
  <xsl:variable name="total-price"
                select="$total-price + (./price * ./amount)"/> 
</xsl:template>
```

##### 6 Repetition

Using xsl:for-each
- Similar to for loops
- Attribute select selects a set of nodes on which the body of element `xsl:for-each` is applied
- Document order, can be `xsl:sort`ed

```xml
<xsl:for-each select=".//item">
  <xsl:call-template name="process-item" />
</xsl:for-each>
```


##### 7 Branching

###### Conditions

Using element `xsl:if` we can execute a part of a template only in case a condition is satisfied
- Attribute test contains a logical XPath condition

Note: It does not have an else branch!!

```xml
<xsl:if test="@dispatched">
  <xsl:text>The order was dispatched on </xsl:text>
  <xsl:value-of select="@dispatched" />
</xsl:if>
```

###### General

Generalization of `xsl:if` is `xsl:choose`
- One of more branches `xsl:when`
  - With attribute test containing the condition
  - Executed, when the condition is satisfied, others are ignored
- One branch `xsl:otherwise`
  - Executed, if no `xsl:when` branch was executed

```xml
<xsl:choose>
  <xsl:when test="@dispatched">
    <xsl:text>The order was dispatched.</xsl:text>
  </xsl:when>
  <xsl:when test="@delivered">
    <xsl:text>The order was delivered.</xsl:text>
  </xsl:when>
  <xsl:otherwise>
    <xsl:text>The order is being processed.</xsl:text>
  </xsl:otherwise>
</xsl:choose>
```

###### Recursion (examples)

```xml
<xsl:template name="total-price">
  <xsl:param name="inter-result" />
  <xsl:param name="item" />
  <xsl:variable name="newinter-result"
                select="$inter-result + ($item/price * $item/amount)" />
  <xsl:choose>
    <xsl:when test="count($item/following-sibling::item)>0">
      <xsl:call-template name="total-price">
        <xsl:with-param name="inter-result" select="$newinter-result" />
        <xsl:with-param name="item" select="$item/following-sibling::item[1]" />
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="value-added-tax">
        <xsl:with-param name="price" select="$newinter-result" />
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
```

### Implicit Templates

An empty XSLT script applied on a nonempty input produces a non-empty output
- Due to implicit templates
- When a node should be transformed and we cannot find a suitable user-specified template, an implicit template is used

3 (6) templates
```xml
<!--if nothing, apply on children (not attributes)-->
<xsl:template match="*|/">
  <xsl:apply-templates/>
</xsl:template>

<!--select values of elements and or attributes-->
<!--empty stylesheet => text values of elements -->
<xsl:template match="text()|@*">
  <xsl:value-of select="."/>
</xsl:template>

<!--do nothing-->
<xsl:template match="processing-instruction()|comment()"/>
```

This will override the implicit ones
```xml
<xsl:template match="node()" />
```



### XSLT programming – Two Approaches

1. Unnamed templates + apply-templates
    - The processing is driven by the XSLT parser searching for the most suitable template 
2. Named templates + for-each + if + choose 
    - The processing is driven by the programmer


Can be combined arbitrarily

### Advanced XSLT

#### XSLT 1.0

##### Sorting

Element `xsl:sort`
- Within `xsl:apply-templates` or `xsl:for-each`
  - Influences the order of further processing 
- Attribute `select`
  - According to what we sort 
- Attribute `order`
  - `ascending` / `descending`
    - Default: `ascending`

##### Keys

Element `xsl:key`
- Attribute `name` 
  - Name of key
- Attribute `match`
  - XPath expression identifying elements for which we define the key
- Attribute `use`
  - XPath expression identifying parts of the key

- Function `key(key-name, key-value)` 
  - Finds the node with key having `key-name` and value `key-value`

- Dereferencing

```xml
<xsl:key name="product-key"
         match="product"
         use="./product-code" />
<xsl:for-each select="//item">
  <xsl:variable name="prod"
                select="key('product-key', ./@code)" />
  <xsl:value-of select="$prod/name" />
  <xsl:value-of select="$prod/vendor" />
</xsl:for-each>
```

##### Modes

Processing of the same nodes in different ways = modes
Attribute mode of element xsl:template and xsl:apply-template
- Only for unnamed templates

```xml
<xsl:template match="/">
  <xsl:apply-templates mode="overview" />
  <xsl:apply-templates mode="full-list" />
</xsl:template>

<xsl:template match="item" mode="overview">
  ... 
</xsl:template>

<xsl:template match="item" mode="full-list">
  ...
</xsl:template>
```

##### Combinations of Scripts

Referencing to another XSLT script

Element `xsl:include`
- Attribute href refers to an included script 
- Anywhere in the script
- The templates are "included" (copied) to the current script

Element `xsl:import` 
- Attribute href refers to an imported script 
- Only beggining of script
- In addition, the rules from the current script have higher priority than the imported ones 
- `xsl:apply-imports` – we want to use the imported templates (with the lower priority)

```xml
<!-- stylesheet A -->
<xsl:stylesheet ...>

  <xsl:import href="C.xsl" />
  <xsl:include href="B.xsl" />
  
</xsl:stylesheet>
```

Last overrides all before (!!!)

##### Copies of Nodes

Element `xsl:copy-of`
- deep copy
- Attribute select refers to the data we want to copy 
- Creates a copy of the node including all child nodes


Element `xsl:copy`
- Creates a copy of the current node, but not its attributes or child nodes

```xml
<xsl:template match="/"> <xsl:copy-of select="."/> </xsl:template>

```
```xml
<xsl:template match="/|@*|*|text()">
  <xsl:copy>
    <xsl:apply-templates select="@*|*|text()"/>
  </xsl:copy>
</xsl:template>
```

- Both create a copy of the input document, but in a different way

#### XSLT 2.0

Uses XPath 2.0
- XSLT 1.0 uses XPath 1.0

Adds new constructs (elements) 
- The output (input) can be into (from) multiple documents
- User-defined functions
- Can be called from XPath expressions
- Element `xsl:for-each-group` for grouping of nodes

…and many other extensions (see http://www.w3.org/TR/xslt20/)

##### Output and Input

Element `xsl:result-document`
- Attribute `href`
  - URL of output document
- Attribute `format`
  - Format of the output document 
  - Reference to an `xsl:output` element 

Element `xsl:output`
- New attribute name 
  - To enable referencing

![0acc8242.png](attachments/15e42996-b386-4d59-87af-840ece336296/0acc8242.png)

##### Grouping of Nodes

Grouping of nodes according to specific conditions

Element `xsl:for-each-group`
- Attribute `select` 
  - Like for `xsl:for-each` 
- Attribute `group-by` 
  - XPath expression specifying values according to which we group 
- … and other attributes for other types of grouping
- Function `current-group()`` returns items in the current group

![1b1b5ca1.png](attachments/15e42996-b386-4d59-87af-840ece336296/1b1b5ca1.png)

##### User-defined Functions

Element `xsl:function`
- Attribute `name`
  - Name of function
- Attribute `as`
  - Return value of function
- Subelement `xsl:param`
  - Parameter of function


- Similar mechanism as named templates
- But we can use the functions in XPath expressions

#### XSLT 3.0

- Currently just W3C candidate recommendation - already false, 2017 was done
- To be used in conjunction with XPath 3.0

Main extensions:
- Streaming mode of transformations
  - Neither the source document nor the result document is ever held in memory in its entirety
  - Motivation: we do not want to load the entire document in memory
- Higher order functions
- Extended text processing
- Improves modularity of large stylesheets
- …


##### Streaming

Restrictions to be aware of:
- We have access only to the current element attributes and namespace declaration 
- Sibling nodes and ancestor siblings are not reachable
- We can visit child nodes only once

![8c9ae164.png](attachments/15e42996-b386-4d59-87af-840ece336296/8c9ae164.png)
![d198eb8e.png](attachments/15e42996-b386-4d59-87af-840ece336296/d198eb8e.png)
- missing explicit support for streaming on template

##### Higher-Order Functions

- Higher order functions = functions that either take functions as parameters or return a function
- XSLT 3.0 introduces the ability to define anonymous functions
  - Enables meta-programming using lambda expressions 

Example:
- `(x, y) → x*x + y*y` … lambda expression that calculates the square of two numbers and sums them
- `x → (y → x*x + y*y)` … equivalent expression that accepts a single input, and as output returns another function, that in turn accepts a single input 

![31c3fde3.png](attachments/15e42996-b386-4d59-87af-840ece336296/31c3fde3.png)

Support for common lambda patterns (operators)
- `map` – applies the given function to every item from the given sequence, returning the concatenation of the resulting sequences
- `filter` – returns items from the given sequence for which the supplied function returns true
- `fold-left` – processes the supplied sequence from left to right, applying the supplied function repeatedly to each item, together with an accumulated result value
- `fold-right` – respectively
- `map-pairs` – applies the given function to successive pairs of items taken one from sequence 1 and one from sequence 2, returning the concatenation of the resulting sequences

![e38e676d.png](attachments/15e42996-b386-4d59-87af-840ece336296/e38e676d.png)

---
## Schemas

### Best Practices

How to define XML schemas for various use cases
- Are we going to use the schema locally or share it with others? 
- Will the schema evolve?
- Are we going to preserve multiple versions of schemas? 

There are many recommendations
- Fact: The W3C specification does not recommend anything

#### Basic Recommendations

Use the XML features fully:
- Use your own elements and attributes
  - `<attribute name="number" value="ORD001" />` - use types
- Maximize readability of XML documents using reasonable element and attribute names
- Even though the names are longer
  - We can use (XML-specific) compression
  - `<e><nm>Martin Necasky</nm></e> `
- Do not use dot notation instead of XML tree hierarchy
  - `<customer.name.first>Martin</customer.name.first>`
- Do not use references instead of tree hierarchy
  - They usually have less efficient processing
  - (Of course, in some cases it might make sense)
- XML data model != relational data model

#### Elements vs. Attributes

- When to use elements and when attributes?
- There is no general rule, but:
  - An attribute can be specified only once for each element
  - An attribute can not have a complex structure
- If we assume new versions of schema, where complex structure or repetition is expected, element is a better choice
  - If we use attributes, we must:
    - Transform all existing documents
    - Change all XPath paths
    - …
  - Repetition can be solved using a multivalue attribute, but its further parsing is more difficult

If we want to prefer value A to value B, we use for A element and for B attribute
- B says something about A, it contains metadata


The decision may also depend on the API we use for data access
- SAX: all attribute values are provided in a single event `startElement`
- May be useful, when we do not want to wait for reading of subelements
- The resulting code will be simpler and more lucid

- Anything that can be an attribute, can be also an element
- In general: Use attributes so that the work with the data is suitable for the particular application

### Namespaces

Use them!
- When you do not use them, you limit others in usage of your schemas

A namespace is identified with a URI
- It is good when you can put there something related
  - Documentation of the schema, examples, references to related schemas, …
  - The schema itself also, but it is not compulsory
- Note: URI does not mean that it is an address of the schema! It is an identifier!

- Usually in a single system you will design multiple related schemas and re-use existing
- In this context there are three key approaches for work with namespaces
  - Homogeneous
    - include
    - If the XML schemas describe domains of different problems
    - If the XML schemas have distinct administrators
      - If I am the owner of the schemas
    - If there may occur a collision of elements / attributes / types
  - Heterogeneous
    - import
    - For XML schemas which are conceptually linked
    - i.e. describe the same problem domain and have a common administrator
    - There are no conflicts among element / attribute names
      - Collision: Two globally defined elements / attributes with the same name but different type 
    - There are no conflicts among types
      - Collision: Two globally defined data types with the same name but different content model
  - Chameleon
    - If the XML schema defines general types which do not have a special semantics or which can be used regardless the problem domain
      - e.g. data types for an address, name, e-mail, …
      - e.g. general data types like array / list of items, string of a particular length, …

Example:
- order.xsd uses
  - customer.xsd
  - product.xsd

![c8545d8c.png](attachments/15e42996-b386-4d59-87af-840ece336296/c8545d8c.png)
![fddb1780.png](attachments/15e42996-b386-4d59-87af-840ece336296/fddb1780.png)
![5405a948.png](attachments/15e42996-b386-4d59-87af-840ece336296/5405a948.png)

### XML Schema Versioning

 Your XML schemas will evolve over time ◼ Users have new requirements ◼ The world is changing ◼ …  If we use XML data format, we usually have to manage multiple versions of each schema  The users must be provided with the version they are interested in and want to use ◼ And, of course, the XML documents valid against the selected version

1st option:
XML Schema attribute version of element schema
- Problem: XML documents do not know this information
  - This version cannot be directly checked in the XML document
- Good for backwards compatibility

```xml
<xsd:schema version="BFLM.PSFVZ">
  <element name="order" type="Order"/>
  ...
</xsd:schema>
```
```xml
<?xml version="1.0"?>
<order ... >
</order>
```

2nd option:
own versioning attribute declared for the root element
- XML documents must have correct version of XML schema in the root element
  - If not, validation reveals it
- Problem: XML documents must change also in case when it is not necessary (the version must change)

```xml
<xsd:schema>
  <xsd:element name="order" type="Order"/>
  <xsd:complexType name="Order">
    ...

    <xsd:attribute name="version" type="string" fixed="AEIO.U"/>
  </xsd:complexType>
...
</xsd:schema
```
```xml
<?xml version="1.0"?>
<order version="AEIO.U" >
  ...
</order>
```

3rd option:
including the number of version into the value of attribute targetNamespace
- XML documents can use the namespace (with particular version) suitable for particular situation
```xml
<xsd:schema targetNamespace= ".../orders/1.0">
  ... 
</xsd:schema>

```
```xml
<?xml version="1.0"?>
<order xsi:schemaLocation=".../orders/1.0 Orders.xsd">
</order>
```

#### Takeaways

- Use versions for your XML schemas 
- Force the XML documents to contain version number of the schema 
  - Somehow 
- Provide the users with all versions of XML schemas
- Specify versioning notation so that the users know which changes influence validity of XML documents
  - E.g. X.Y, where:
    - New Y means that XML documents valid against X.Y-1 ARE valid against X.Y
    - New X means that XML documents valid against X-1.Y DO NOT HAVE TO BE valid against X.0


Another problem:
- The provider of XML data has several versions of XML schemas (1, …, n)
- The consumer wants XML data valid against version i 
- The provider does not want to store the XML data in all possible versions
  - Inefficient (space requirements, management, …)

Solution:
- Internal representation such that:
  - The data are stored only once 
  - We can export the data into the version required by user
    - Using XSLT | XQuery | SQL/XML

![fb5c2715.png](attachments/15e42996-b386-4d59-87af-840ece336296/fb5c2715.png)

### Extensible Content Types

Solve being non-deterministic

- If `extension` is insufficient, use `any`
BUT:
```xml
<complexType name="Publication">
  <sequence>
  <element name="name"/>
  <element name="price"/>
  <any namespace="##any" minOccurs="0"
       maxOccurs="unbounded"/>
  <element name="publisher"/> 
  </sequence>
</complexType>
```
When the parser validates publisher, it does not know where it is defined


Solution -encapsulate
```xml
<complexType name="Publication">
  <sequence> <element name="name"/>
    <element name="price"/>
    <element name="other">
      <complexType>
        <sequence>
          <any .../>
        </sequence> 
      </complexType>
    </element> 
    <element name="publisher"/>
  </sequence> 
</complexType>

```


### Conclusion

The provided set of cases was not complete, but a set of motivating examples
- There are numerous other best practices, recommendations etc.
  - More or less reasonable 

General advice: If you design a set of XML schemas, first think about the particular application and consequences

---
## RELAX NG

### General features

- Results from two older languages
- Based on the idea of patterns
  - RELAX NG schema = a pattern of XML document
  - Note: XML Schema is considered to be based on types


- Simple and easy-to-learn
- Has two types of syntaxes: XML a compact (non-XML)
  - Mutually convertible
  - XML Schema does not have a compact version
- Supports namespaces
  - DTD does not
- Has unlimited support for unordered sequences
  - XML Schema does not
- Has unlimited support for mixed content
  - DTD does not
- It enables to use a wide set of simple data types
  - e.g. from XML Schema

### XML vs. Compact Syntax

![7331ee34.png](attachments/15e42996-b386-4d59-87af-840ece336296/7331ee34.png)
![240d03b4.png](attachments/15e42996-b386-4d59-87af-840ece336296/240d03b4.png)
![deb52137.png](attachments/15e42996-b386-4d59-87af-840ece336296/deb52137.png)
![94413ef6.png](attachments/15e42996-b386-4d59-87af-840ece336296/94413ef6.png)
![16325ec5.png](attachments/15e42996-b386-4d59-87af-840ece336296/16325ec5.png)
![7a0d13ca.png](attachments/15e42996-b386-4d59-87af-840ece336296/7a0d13ca.png)
![c4fb5e21.png](attachments/15e42996-b386-4d59-87af-840ece336296/c4fb5e21.png)
![4f9bf6b6.png](attachments/15e42996-b386-4d59-87af-840ece336296/4f9bf6b6.png)
![c8cec0a1.png](attachments/15e42996-b386-4d59-87af-840ece336296/c8cec0a1.png)
![76d91884.png](attachments/15e42996-b386-4d59-87af-840ece336296/76d91884.png)
![3918600a.png](attachments/15e42996-b386-4d59-87af-840ece336296/3918600a.png)

### Data Types

Built-in: 
- string and token

Typically we use XML Schema data type
![2e4fb20c.png](attachments/15e42996-b386-4d59-87af-840ece336296/2e4fb20c.png)
![f4f71708.png](attachments/15e42996-b386-4d59-87af-840ece336296/f4f71708.png)
![86417ad3.png](attachments/15e42996-b386-4d59-87af-840ece336296/86417ad3.png)
![80c1cd94.png](attachments/15e42996-b386-4d59-87af-840ece336296/80c1cd94.png)

###### Example
![5b1cbac7.png](attachments/15e42996-b386-4d59-87af-840ece336296/5b1cbac7.png)


### Conclusion

Other schema components:
- Naming of schema parts + repeatable usage
- Usage of namespaces
- Documentation and comments

---
## Schematron

Based on the idea of patterns
- Similar meaning as in XSLT
- Does not define a grammar 
  - DTD, XML Schema, RELAX NG are grammar-based 

Defines a set of rules which must be followed by a valid XML document
- Expressed using XPath 
- For validation we can use an XSLT processor


### Structure

Root element `<schema xmlns="http://purl.oclc.org/dsdl/schematron">` contains:
- `<title>` – name of schema (optional) 
- `<ns prefix="…" uri="…" />` – declarations of namespace prefixes (arbitrary amount) 
- `<pattern>` – (at least one) pattern containing: 
  - `<rule context="…">` – (at least one) rule applied in the context and containing subelements: 
    - `<assert test="…">` – if XPath expression in attribute test does not return true, the result of validation is the content of assert 
    - `<report test="…">` – if XPath expression in attribute test does return true, the result of validation is the content of report


![30731bf7.png](attachments/15e42996-b386-4d59-87af-840ece336296/30731bf7.png)

![fa711ff8.png](attachments/15e42996-b386-4d59-87af-840ece336296/fa711ff8.png)

### Validation

Option A: 
- Special SW / library for Schematron

Option B: 
- Arbitrary XSLT processor 
  - There exists an XSLT script which transforms Schematron schemas to XSLT scripts
  - The resulting XSLT script is applied on XML documents 
  - The result is content of assert a report elements

### Combination with XML Schema

Validation:
- Option A: Special SW / library for Schematron
- Option B: Using XSLT we extract a Schematron schema and validate it

![9f62cfb8.png](attachments/15e42996-b386-4d59-87af-840ece336296/9f62cfb8.png)

### Combination with RELAX NG

There exists also a compact non-XML version:
 Validation: ◼ Option A: Special SW / library for Schematron ◼ Option B: Using XSLT we extract a Schematron schema and validate it

```schematron
namespace sch = "http://www.ascc.net/xml/schematron"
[ sch:pattern [ name = " Do we have enough for salaries? " 
  sch:rule [ context = "employees" 
    sch:report [ test = "sum(employee/salary) > 50000" 
        "The sum of salaries can not be greater than 50.000." ] ] ] ]
```

![b0ab9a19.png](attachments/15e42996-b386-4d59-87af-840ece336296/b0ab9a19.png)

---
## XQuery

XML Query Languages
- Aims: querying, views, updates
- Since 1998 XML-QL, XQL, …
  - W3C specifications: XSLT 1.0, 2.0, 3.0, … XPath 1.0, 2.0, 3.0, XQuery 1.0, 3.0
    - XPath (1.0) – selecting of parts of the tree
    - XSLT – data transformations
    - XQuery – XML querying (user-oriented syntax)

- The same data model as XPath 2.0
  - XPath 2.0 subset of XQuery
  - Each XPath 2.0 query is also a query in XQuery
- XPath 1.0 and 2.0 (and hence XQuery 1.0) are not fully compatible
  - Different subsets of XML Infoset

- Higher expressive power than XPath 2.0, XQL, etc.
- Clear semantics (XQuery Core model)
  - See later
- Exploitation of XML Schema
  - Description of structure 
  - Data types 
- Compatibility of data model with XML Infoset 

```xml
<?xml version="1.0"?>
<catalogue>
  <book year="2002">
    <title>The Naked Chef</title>
    <author>Jamie Oliver</author>
    <isbn>0-7868-6617-9</isbn>
    <category>cook book</category>
    <pages>250</pages>
  </book>
  <book year="2007">
    <title>Blue, not Green Planet</title>
    <subtitle>What is Endangered? Climate or Freedom?</subtitle>
    <author>Václav Klaus</author>
    <isbn>978-80-7363-152-9</isbn>
    <category>society</category>
    <category>ecology</category>
    <pages>176</pages>
  </book>
  ...
</catalogue>
```

- XQuery is a functional language
  - Query is an expression
  - Expressions can be combined
- XQuery query:
  - (Optional) declaration of namespaces
  - (Optional) definition of functions
  - Query itself

### Constructs

- XPath expressions
  - `//catalogue/book[author="Jamie Oliver"]`
- Constructor 
  - `element book {element author}`
- FLWOR expression 
  - `FOR ... LET ... WHERE ... ORDER BY ... RETURN `
- Conditional expression
  - `IF ... THEN ... ELSE`
- Quantifiers 
  - `EVERY $var IN expr SATISFIES expr`
  - `SOME $var IN expr SATISFIES expr` 
- Type operator 
  - `TYPESWITCH typeexpr CASE ... DEFAULT `
- Operators and functions 
  - `x + y, z = x, func(x,y,z) `
- Variables and constants
  - `$x, "Obama", 256`
- Comparison

#### Constructors

- Direct constructors
  - `{}` only in bodz of element or as attribute value
    - so not e.g. compute name of element

```xquery
<html> 
  <body>
    <h1>Listing from doc("catalogue.xml")//book</h1> 
    <h2>title: {doc("catalogue.xml")//book[1]/title}</h2> 
    <h3>subtitle: {doc("catalogue.xml")//book[1]/subtitle}</h3> 
    <h2>
      title: {fn:data(doc("catalogue.xml")//book[2]/title)} 
    </h2>
    <h3> 
      subtitle: {fn:data(doc("catalogue.xml")//book[2]/subtitle)} 
    </h3>
  </body>
</html>
```

- Computed constructors
  - can compute even name of element

```xquery
element html {
  element body {
    element h1 {"Listing from doc(‘catalogue.xml’)//book"}, 
    element h2 {
      text{"title: "}, 
      doc("catalogue.xml")//book[1]/title 
    }
    ...
  }
} 
```

#### FLWOR

##### Parts

- at least one for or let and one return


- Basic XQuery construction
  - Like `SELECT-FROM-WHERE-…` in SQL
- Clause for `(for $var in expr)` (FLWOR)
  - Evaluates expression expr whose result is a sequence 
    - see XPath 2.0 data model
  - Iteratively assigned to variable $var
- Clause `let (let $var := expr)` (FLWOR)
  - Evaluates expression expr and assigns the result to variable $var
- Clause `where (where expr)` (FLWOR)
  - Filters sequences from clause for
- Clause `order by (order by expr)` (FLWOR)
  - Sorts sequences filtered by clause where according to the given criterion
- Clause `return (return expr)` (FLWOR) 
  - Concluding clause which constructs the result of the query from the selected, filtered and sorted sequences

> For each book with more than 300 pages return the title and author sorted by year of edition
```xquery
for $book in doc("catalogue.xml")//book 
where $book/pages > 300
order by $book/@year
return
  <book>
    {$book/title}
    {$book/author}
  </book>
```

##### Use

FLWOR expressions enable to transform the original structure of the data ◼ e.g., transformation to XHTML and other formats

Examples:
- XHTML table of books
- Swapping of parent/child elements
  - book / author → author / list of books
- Grouping
  - Grouping of books according to categories
- Joining of data from different resources 
  - We extend the books in the catalogue with reviews from another resource

#### Conditions

- Clause if (if expr)
  - Evaluates expression expr whose value is true/false
- Clause then (then expr) 
- Clause else (else expr)

#### Quantifiers

Clause every/some (every/some $var in expr)
- Evaluates expression expr and requires that each/some of the sequences in its result satisfies the condition 

Clause satisfies (satisfies expr)
- expr is the condition of the quantifier

> Return the authors from document authors.xml, who write only books which are translated (i.e. have an original)

```xquery
for $author in distinct-values(doc("authors.xml")//author) 
where every $author-book in
    for $book in doc("catalogue.xml")//book 
    where $book[author = $author/name] 
    return $book 
  satisfies 
    $author-book/original
return $author
```

#### Functions

Built-in functions
- distinct-values, empty, name, ... 
- Aggregation functions max, min, avg, count, ...
- Other: string, numeric and other data types
  - A huge number
- Namespace fn
  - URI: http://www.w3.org/2005/xpath-functions

User-defined functions
- Defined using the XQuery syntax 
- Typed, recursive, … 
- Support for libraries 
- Syntax
  - `declare function name(parameters) as type`
    - name = name of the function
    - parameters = list of parameters
      - Typed/untyped
    - type = type of return value


###### Examples
- Document node related to the specified uri: `fn:doc($uri as xs:string?) as documentnode()?`
- Sequence of atomic values of the given sequence of items `fn:data($arg as item()*) as xs:anyAtomicType*`
- Number of items in a sequence `fn:count($arg as item()*) as xs:integer `
- Removing of duplicities (only for atomic values) `fn:distinct-values($arg as xs:anyAtomicType*) as xs:anyAtomicType*`

##### Imports

![d99cb7f8.png](attachments/15e42996-b386-4d59-87af-840ece336296/d99cb7f8.png)
![d0c6314c.png](attachments/15e42996-b386-4d59-87af-840ece336296/d0c6314c.png)

#### Comparison

##### Value

Operators 
- lt, "less than"
- gt, "greater than"
- le, "less or equal"
- ge, "greater or equal"
- eq, "equal"
- ne, "non equal"

Algorithm: 
- Atomization
  - Atomic value
- Implicit conversion to the same data type
- Comparison of the operands


- Untyped operands are implicitly converted to strings
- If any of the operands is converted to an empty sequence, the result is an empty sequence
- If any of the operands is converted to a sequence longer than 1, error

![a64f24fa.png](attachments/15e42996-b386-4d59-87af-840ece336296/a64f24fa.png)

##### General (Group)

- Operators
  - \<, \>, \<=, \>=, =, !=
- Also for sequences
- Algorithm:
  - Atomization
    - The result are sequences of atomic values 
  - Searching for an (at least one is enough) item from left and right operands which evaluate to true 
    - If there exists such pair, true 
    - Otherwise, false

When searching a pair of items, again a conversion:
- Both untyped – conversion to xs:string
- One untyped, other numeric – conversion to xs:double
- One untyped, other typed, but other than numeric/string – conversion to the particular type

![9445ffc3.png](attachments/15e42996-b386-4d59-87af-840ece336296/9445ffc3.png)

##### Node

Operators
- is, \<\< and \>\>

Algorithm: 
- Evaluation of operands 
- If one of the operands is an empty sequence, the result is an empty sequence 
- If any of the operands returns a sequence longer than 1, error 
- Otherwise: 
  - is returns true if both operands are nodes with the same identity 
  - \<\< returns true if the left operand precedes the right operand (in the document   order) 
  - \>\> returns true if the left operand follows the right operand (in the document order)

![4b6db5d5.png](attachments/15e42996-b386-4d59-87af-840ece336296/4b6db5d5.png)

#### Integrity Constraints

- XML Schema can be used as a tool for specification of various integrity constraints (ICs)
  - e.g. cardinalities, keys, data types, …
  - Version 1.1: element assert (using XPath)
- It does not provide a robust tool for specification of more complex ICs 
  - e.g. "If an author does not write in Czech, each of their books must contain also a title in their original language and the name of translator."


- XQuery is a sufficiently powerful language for expressing ICs
  - Like the CHECK constraint in SQL
- The IC is expressed as a query which returns a warning if necessary, e.g.
  - If the data are OK 
    - `<ok no="number of IC"/>`
  - If an IC is violated 
    - `<error no="number of IC">warning text</error>`

![553ac235.png](attachments/15e42996-b386-4d59-87af-840ece336296/553ac235.png)

### Support for Schemas

- Support for schemas is an important extension of query languages
  - XQuery must be able to work with documents without a schema
  - XQuery must exploit the schema if it exists
  - The implementation can allow static typing and detect and report type errors
- The type system is based on XML Schema

```xquery
typeswitch($customer/billing-address)
  case $a as element(*, USAddress) return $a/state
  case $a as element(*, CanadaAddress) return $a/province
  case $a as element(*, JapanAddress) return $a/prefecture
  default return "unknown"
```

```xquery
5      instance of xs:decimal 
(5, 6) instance of xs:integer+ 
.      instance of element()

```

### Semantics

- XQuery contains a huge amount of redundancies
- XQuery Core defines a syntactic subset of XQuery with the same expressive power as XQuery, but without duplicities
  - The definition involves also rules for re-writing of queries into XQuery Core
- XQuery Core is useful mainly from the theoretical point of view 
  - The queries are long and complex

### Advanced XQuery

#### XQuery Update Facility 1.0

Extension of XQuery 1.0
- Node insertion
- Node deletion
- Node modification (preserving identity)
- Creating a modified copy of a node

Assumes that the data are persistently stored in a database, file system, … 
- We change the stored data

#### (Dotazovaci vyrazy vs) Aktualizacni vyrazy

##### Construct insert

- Inserts copies of zero or more source nodes at a position specified with regards to a target node
  - Source nodes: SourceExpr (dotazovaci vyraz)
  - Target node: TargetExpr (dotazovaci vyraz)
- Instead of node we can use nodes (it does not matter)

```xquery
insert node SourceExpr into TargetExpr
insert node SourceExpr as first into TargetExpr
insert node SourceExpr as last into TargetExpr
insert node SourceExpr after TargetExpr
insert node SourceExpr before TargetExpr
```



Conditions:
- SourceExpr and TargetExpr cannot be update expressions
- For the into versions TargetExpr must return exactly one element / document node
- For other versions TargetExpr must return exactly one element / text / comment / processing instruction node

```xquery
insert node <phone>111111111</phone> after //customer/email
insert node <phone>111111111</phone> into //customer
```

##### Construct delete

`delete node TargetExpr`

- Deletes target nodes
  - Specified using TargetExpr
- Instead of node we can use nodes (it does not matter)

Conditions:
- TargetExpr cannot be an update expression
- TargetExpr must return zero or more nodes


- If any of the target nodes does not have a parent, it depends on the implementation whether an error is raise

```xquery
delete node //customer/email
delete node //order[@status = "dispatched"]
```

##### Construct replace

`replace node TargetExpr with Expr`

Replaces the target node with a sequence of zero or more nodes
- Target node: TargetExpr

Conditions:
- TargetExpr cannot be update expressions
- TargetExpr must return a single node and must have a parent



##### Construct replace value of

`replace value of node TargetExpr with Expr`

Modifies the value of the target node
- Target node: TargetExpr

Conditions:
- TargetExpr cannot be update expressions 
- TargetExpr must return a single node

##### Other constructs

Renaming
- `rename node TargetExpr as NewNameExpr`

Transformation
- Creating a modified copy of a node (having a new identity)
```xquery
copy $VarName := ExprSource
modify ExprUpdate
return Expr
```

#### Special Namespaces

#### XQuery Data Model

##### Atomic Values

##### Nodes

##### Result

#### XQuery 3.0

- Group by clause in FLWOR expressions
- Tumbling (sliding) window and sliding window in FLWOR expressions 
  - Iterates over a sequence of tuples (overlapping or not)
- Expressions try / catch 
- Dynamic function call
  - Function provided as a parameter 
- Public / private functions 
- … 
- XQuery Update Facility 3.0


---

## XQuery vs. XSLT

- XSLT = language for XML data transformation
  - Input: XML document + XSLT script
  - Output: XML document
    - Not necessarily
- XQuery = language for XML data querying
  - Input: XML document + XQuery query
  - Output: XML document
    - Not necessarily
- Seem to be two distinct languages
  - Observation: Many commonalities
  - Problem: Which of the languages should be used?

### Examples

![4ebd4014.png](attachments/15e42996-b386-4d59-87af-840ece336296/4ebd4014.png)
![95e9b5ac.png](attachments/15e42996-b386-4d59-87af-840ece336296/95e9b5ac.png)
![0bbe2f75.png](attachments/15e42996-b386-4d59-87af-840ece336296/0bbe2f75.png)
skipped:
![0d8f1a69.png](attachments/15e42996-b386-4d59-87af-840ece336296/0d8f1a69.png)
![fdb60e2b.png](attachments/15e42996-b386-4d59-87af-840ece336296/fdb60e2b.png)
![2aa84f63.png](attachments/15e42996-b386-4d59-87af-840ece336296/2aa84f63.png)
![6ac048ee.png](attachments/15e42996-b386-4d59-87af-840ece336296/6ac048ee.png)

### Takeawys

- In general: It does not matter 

More precisely:
- It depends on the application 

Rules: 
- If the data are stored in database 
- XQuery 
- If we want to copy the document with only small changes 
- XSLT 
- If we want to extract only a small part of the data 
- XQuery
- XQuery is easy-to-learn and simpler for smaller tasks 
- Highly structured data  XQuery 
- Large applications, re-usable components 
- XSLT

---
## Parsers

Problem: We want to process XML data
- Read it in a particular SW


- XML document = text document → we can read the document as a text
  - Demanding, user-unfriendly, inefficient,…
- Solution: While processing XML data, we need to know what is element, attribute, text, comment, … → we are interested in Infoset of the XML document
- XML parser = SW which provides an application with an interface to the Infoset of input XML data 

![e75be396.png](attachments/15e42996-b386-4d59-87af-840ece336296/e75be396.png)

### Types of parsers

1. Sequential
    - Fast, require less memory
    - A single linear traversal of the data
    - Push vs. pull parser
    - A stream of events vs. reading when required
2. Tree representations
    - The whole document is read into memory
    - Repeatable and non-sequential traversal
    - Memory requirements, inefficient


1. Validating 
    - Can check validity of the data against an XML schema
2. Non-validating

![760b555c.png](attachments/15e42996-b386-4d59-87af-840ece336296/760b555c.png)

1. With(out (2.)) support for PSVI



### SAX

~ Simple API for XML

Reading a part of document = event
- We can define a handler 

![555e5920.png](attachments/15e42996-b386-4d59-87af-840ece336296/555e5920.png)

#### Interface - ContentHandler

- Attributes are a part of parameters of `startElement()`

```java
void startDocument () 
void endDocument () 
void startElement (String uri, String localName, String qName, Attributes atts)
void endElement (String uri, String localName, String qName) 
void characters (char[] ch, int start, int length) 
void processingInstruction (String target, String data) 
void ignorableWhitespace (char[] ch, int start, int length)
void startPrefixMapping (String prefix, String uri) 
void endPrefixMapping (String prefix) 
void skippedEntity (String name) 
void setDocumentLocator (Locator locator)
```

##### startElement()

- String uri
  - URI of element namespace
- String localName
  - Local name
- String qName
  - Qualified name
- Attributes atts

```java
for (int i = 0; i < atts.getLength (); i++ ) 
{ 
  System.out.println (atts.getQName (i));
  System.out.println (atts.getValue (i)); 
}
```

##### Interface attributes

- `int getLength ()`
  - Returns the number of attributes in the list of attributes
- `int getIndex (String qName)`
  - Returns the index of attribute with the given (qualified) name 
- `int getIndex (String uri, String localName)`
  - Returns the index of attribute with the given local name and URI of namespace
- `String getLocalName (int index)`
  - Returns the local name of attribute with the given index
- `String getQName (int index)`
  - Returns the qualified name of attribute with the given index
- `String getURI (int index)`
  - Returns the URI of attribute with the given index
- `String getType (int index) `
  - Returns the type of attribute with the given index 
- `String getType (String qName) `
  - Returns the type of attribute with the given (qualified) name 
- `String getType (String uri, String localName) `
  - Returns the type of attribute with the given local name and URI of namespace
- `String getValue (int index) `
  - Returns the value of attribute with the given index 
- `String getValue (String qName) `
  - Returns the value of attribute with the given (qualified) name 
- `String getValue (String uri, String localName) `
  - Returns the value of attribute with the given local name and URI of namespace

##### characters()

SAX parser can buffer the character data arbitrarily → we cannot rely on getting the whole text in a single call of the function
- `char[] ch`
  - An array where the character data are stored
- `int start`
  - Starting position of the characters in the array 
- `int length`
  - Number of characters in the array

##### ignorableWhitespace()

Ignorable white spaces 
- `char[] ch` 
  - An array where the character data are stored 
- `int start` 
  - Starting position of the characters in the array 
- `int length` 
  - Number of characters in the array

##### setDocumentLocator()

```java
class myContentHandler implements ContentHandler {
  Locator locator;

  public void setDocumentLocator (Locator locator) {
    this.locator = locator;
    }
  ...
```

Targeting the place in the document where the event occurred
- Interface Locator
  - `int getColumnNumber ()` – column number
  - `int getLineNumber ()` – row number
  - `String getPublicId ()` – public identifier of the document (if exists)
  - `String getSystemId ()` – system identifier of the document (if exists)


#### Initialization

```java
// Creating of an instance of the parser
XMLReader parser = XMLReaderFactory.createXMLReader ();

// Creating of input stream of data
InputSource source = new InputSource ("myDocument.xml");

// Setting our own content handler for processing of events 
parser.setContentHandler (new myContentHandler ());

// Processing of the data
parser.parse (source);
```

#### StAX

~ Streaming API for XML
- http://stax.codehaus.org/Home

Advantages:
  - DOM: Data traversal is driven by the application; support for data modification 
  - SAX: Low memory requirements
  - StAX: Both 

General properties: 
- Pull parser 
  - The application does not have to save the context, it decides when to move further 
- Idea: cursor which we can move through the data 
  - Raw vs. object-based data access

- developer can build the tree if needed
- 
##### Initialization

```java
while (eventReader.hasNext()) { 
  XMLEvent event = eventReader.nextEvent(); 
  
  if (event.getEventType() == XMLStreamConstants.START_ELEMENT) { 
    StartElement startElement = event.asStartElement(); 
    System.out.println(startElement.getName().getLocalPart());
  }
}
```

Events: ATTRIBUTE, CDATA, CHARACTERS, COMMENT, DTD, END_DOCUMENT, END_ELEMENT, ENTITY_DECLARATION, ENTITY_REFERENCE, NAMESPACE, NOTATION_DECLARATION, PROCESSING_INSTRUCTION, SPACE, START_DOCUMENT, START_ELEMENT 

- start element doesnt have attributes - it is a separate event

##### XMLEventReader

XMLEvent
- Reads XML data
- Knows where we are in the document
  - Column, row 
- Can be transformed to particular (XML) object: 
  - asStartElement – element name, attribute, namespaces
  - asEndElement – element name, namespaces
  - asCharacters – text data 
    - CDATA sections, white spaces, ignorable white spaces, …


##### XMLEventWriter

```java
// we create XMLOutputFactory
XMLOutputFactory factory = XMLOutputFactory.newInstance();
// we create serializer of events
XMLEventWriter writer = factory.createXMLEventWriter (new FileWriter("myData2.xml"));
// we create XMLEventFactory to create events
XMLEventFactory eventFactory = XMLEventFactory.newInstance();


XMLEvent event = eventFactory.createStartDocument();
writer.add(event);

event = eventFactory.createStartElement ("employee", "http://mynamespace.com", “mns");
writer.add(event);

event = eventFactory.createNamespace ("mns", "http://mynamespace.com"); 
writer.add(event);

event = eventFactory.createAttribute ("number", "1234");
writer.add(event);

event = eventFactory.createEndElement ("employee", "http://mynamespace.com", “mns");
writer.add(event);

writer.flush(); 
writer.close();
```

##### XMLStreamReader

```java
while (streamReader.hasNext()) {
  streamReader.next(); 

  if (streamReader.getEventType() == XMLStreamReader.START_ELEMENT) { 
    System.out.println(streamReader.getLocalName());
  }
} 
```

Main difference: When we move cursor (next()), we loose information on the previous event
- XMLEventReader returns the event as an object – we can store it

##### XMLStreamWriter

```java
XMLOutputFactory factory = XMLOutputFactory.newInstance();
XMLStreamWriter writer = factory.createXMLStreamWriter( new FileWriter("myData2.xml"));

writer.writeStartDocument();
writer.writeStartElement("employee");
writer.writeStartElement("data");
writer.writeAttribute("number", "1234");
writer.writeEndElement();
writer.writeEndElement();
writer.writeEndDocument(); 
writer.flush();
writer.close();
```

###  DOM

~ Document Object Model 

W3C standard
- Versions: Level (0), 1, 2, 3
  - Level 0 = DOM-like technologies before standardization by W3C 

- The whole document is loaded into memory
- Tree representation
- Nodes of the tree are represented as objects
  - Document, document fragment, DTD declaration, element, attribute, text, CDATA section, comment, entity, entity reference, notation, processing instruction 
  - Methods of objects are given by the DOM specification 
  - Child nodes of objects are given by XML Infoset


![2194e65a.png](attachments/15e42996-b386-4d59-87af-840ece336296/2194e65a.png)

#### Initialization

Load, parse, process
```java
// DocumentBuilderFactory creates DOM parsers
DocumentBuilderFactory dbf = documentBuilderFactory.newInstance ();

// we do not want to validate (and other parameters can be set)
dbf.setValidating (false);

// we create a DOM parser
DocumentBuilder builder = dbf.newDocumentBuilder ("myDocument.xml");

// the parser processes the documents and builds the tree
Document doc = builder.parse ();

// we process the DOM tree
processTree (doc);
```

Store
```java
// TransformerFactory creates DOM serializers
TransformerFactory tf = TransformerFactory.newInstance ();

// Transformer serializes DOM trees 
Transformer writer = tf.newTransformer ();

// we set encoding
writer.setOutputProperty (OutputKeys.ENCODING, "windows-1250");

// we start transformation of DOM tree into a document
writer.transform (new DOMSource (doc), new StreamResult (new File ("outputDocument.xml")));
```

#### Hierarchy

![0646232e.png](attachments/15e42996-b386-4d59-87af-840ece336296/0646232e.png)

| Node                  | Child Nodes                                                                       |
| --------------------- | --------------------------------------------------------------------------------- |
| Document              | Element (at most one), ProcessingInstruction, Comment, DocumentType (at most one) |
| DocumentFragment      | Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference      |
| Element               | Element, Text, Comment, ProcessingInstruction, CDATASection, EntityReference      |
| Attr                  | Text, EntityReference                                                             |
| Text                  |                                                                                   |
| CharacterData         |                                                                                   |
| ProcessingInstruction |                                                                                   |
| Comment               |                                                                                   |
| CDATASection          |                                                                                   |
| Entity                | Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference      |
| EntityReference       | Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference      |
| Notation              |                                                                                   |
| DocumentType          |                                                                                   |

#### Interface Node

- `String getNodeName ()` - good for attribute or element
- `short getNodeType ()` - type
- `String getNodeValue () ` - value


- `String getBaseURI ()`
- `String getPrefix ()`
- `void setPrefix (String prefix)`
- `String getLocalName ()` - local name
- `String getNamespaceURI () `
- `String lookupNamespaceURI (String prefix) `
- `String lookupPrefix (String namespaceURI)`
- `boolean isDefaultNamespace (String namespaceURI) `


- `boolean hasAttributes ()` 
- `boolean hasChildNodes ()`


- `Node getParentNode () ` - parent
- `Node getPreviousSibling ()` - siblings
- `Node getNextSibling ()`- siblings
- `NodeList getChildNodes ()`- child
- `Node getFirstChild ()`
- `Node getLastChild () `


- `NamedNodeMap getAttributes ()` - attributes
- `String getTextContent ()`
- `Document getOwnerDocument ()`
  - Returns Document associated with the node


- `Node removeChild (Node oldChild)` - remove 
- `Node replaceChild (Node newChild, Node oldChild) ` - replace
- `Node appendChild (Node newChild)` - append/add
- `Node insertBefore (Node newChild, Node refChild) ` - insert

- `Node cloneNode (boolean deep)` - clone
- `void setNodeValue (String nodeValue)`
- `void setTextContent (String textContent) `


- `void normalize ()` - "cleanup"
  - Normalizes all text sub-nodes, i.e. merges text contents and eliminates the empty ones


- `boolean isEqualNode (Node arg)`
- `boolean isSameNode (Node other)` - xpath `is`
- `short compareDocumentPosition (Node other)`
  - Compares positions of Nodes in the document


- can add user data to a node in the tree - only while processing, not serialized to .xml file
- `Object getUserData (String key)`
  - Returns an Object associated with key 
- `Object setUserData (String key, Object data, UserDataHandler handler)`
  - Associates an Object with a key
  - Handler is a callback which will be called any time the node is being cloned, imported, renamed, as well as if deleted or adopted 


- `Object getFeature (String feature, String version)`
  - Returns an Object with a given feature with a given version
- `boolean isSupported (String feature, String version)`
  - Tests if the implementation supports the given feature with the given version

![b788b262.png](attachments/15e42996-b386-4d59-87af-840ece336296/b788b262.png)

##### Examples of processing Child Nodes vs. Attributes

```java
for (Node child = n.getFirstChild();
     child != null;
     child = child.getNextSibling())
   {
     processChildNode(child); 
   }

NamedNodeMap atts = n.getAttributes();
for (int i = 0; i < atts.getLength(); i++)
  { 
    Node att = atts.item(i);
    processAttribute(att);
  }
```

#### Interface Document

- `Attr createAttribute (String name)` - create attr
- `Attr createAttributeNS (String namespaceURI, String qualifiedName)`
- `CDATASection createCDATASection (String data)`
- `Comment createComment (String data)`
- `DocumentFragment createDocumentFragment () `
- `Element createElement (String tagName) ` - create element
- `Element createElementNS (String namespaceURI, String qualifiedName) `
- `EntityReference createEntityReference (String name) `
- `ProcessingInstruction createProcessingInstruction (String target, String data) `
- `Text createTextNode (String data) ` - create text node


- `Element getElementById (String elementId)` - get by ID
  - Returns an element with a given value of attribute of type ID 
- `NodeList getElementsByTagName (String tagname)` - get by tag name
- `NodeList getElementsByTagNameNS (String namespaceURI, String localName)` 


- `Element getDocumentElement ()`
- `DocumentType getDoctype () `


- `Node renameNode (Node n, String namespaceURI, String qualifiedName)`
- `Node adoptNode (Node source) `
  - Appends Node to current document
- `Node importNode (Node importedNode, boolean deep)`
  - Imports a node to current document, i.e. creates its copy


- `String getInputEncoding ()`
  - Returns encoding used when parsing
- `String getXmlEncoding () ` - get encoding
- `DOMImplementation getImplementation ()`
  - Returns implementation (DOMImplementation) associated with the document
- `DOMConfiguration getDomConfig ()`
  - Returns configuration for normalization of nodes 


- `boolean getXmlStandalone () `
- `String getXmlVersion ()` - get version
- `String getDocumentURI ()`
- `void setXmlStandalone (boolean xmlStandalone)`
- `void setXmlVersion (String xmlVersion)`
- `void setDocumentURI (String documentURI) `


- `void normalizeDocument ()` - "cleanup"
  - Normalizes XML document, i.e. replaces all references to entities and normalizes text values


- `boolean getStrictErrorChecking ()`
  - Checks whether error checking is given by the specification or depends on the implementation
- `void setStrictErrorChecking (boolean strictErrorChecking)`
  - Sets whether error checking is given by the specification or depends on the implementation

#### Interface Element

- `String getTagName ()` - name
  - Returns the element name

- `NodeList getElementsByTagName (String name)` - in sub-tree
  - Returns the NodeList of all child elements with the given name
- `NodeList getElementsByTagNameNS (String namespaceURI, String localName)
  - Returns the NodeList of all child elements with the given local name and URI

- `String getAttribute (String name)` - attr
  - Returns the value of attribute with the given name
- `Attr getAttributeNode (String name)`
  - Returns the attribute with the given name
- `Attr getAttributeNodeNS (String namespaceURI, String localName)`
  - Returns the attribute with the given local name and URI
- `String getAttributeNS (String namespaceURI, String localName)`
  - Returns the value of attribute with the given local name and URI


- `boolean hasAttribute (String name)`
- true = the element has an attribute wit the given name
- `boolean hasAttributeNS (String namespaceURI, String localName)`
- true = the element has and attribute with the given local name and URI

- `void removeAttribute (String name)`
- Removes attribute with the given name
- `Attr removeAttributeNode (Attr oldAttr)`
- Removes the given attribute node
- `void removeAttributeNS (String namespaceURI, String localName)`
- Removes attribute with the given local name and URI

- `TypeInfo getSchemaTypeInfo ()`
- Type information for the given element

- `void setAttribute (String name, String value)`
- Adds a new attribute with the given name and value 
- `Attr setAttributeNode (Attr newAttr) `
- Adds a new attribute node, replaces if it exists 
- `Attr setAttributeNodeNS (Attr newAttr) `
- Adds a new attribute node, replaces if it exists and takes into account also namespaces
- `void setAttributeNS (String namespaceURI, String qualifiedName, String value)`
- Adds a new attribute with the specified parameters

- `void setIdAttribute (String name, boolean isId)`
- Changes attribute type from/to ID 
- `void setIdAttributeNode (Attr idAttr, boolean isId)`
- Changes attribute type from/to ID`
- `void setIdAttributeNS (String namespaceURI, String localName, boolean isId)`
- Changes attribute type from/to ID

```java
public Node createEmployee(Document document) {
  Element firstName = document.createElement("FirstName"); 
  firstName.appendChild(document.createTextNode("Shawn"));

  Element lastName = document.createElement("LastName"); 
  lastName.appendChild(document.createTextNode("Michaels"));

  Attr genderAttribute = document.createAttribute("gender"); 
  genderAttribute.setValue("M");

  Element employee = document.createElement("Employee"); 
  employee.setAttributeNode(genderAttribute); employee.appendChild(firstName); 
  employee.appendChild(lastName);
  
  return employee; 
  }
```

#### Interface Attr

- `String getName () ` - name
  - Returns attribute name 
- `String getValue () ` - value
  - Returns attribute value 
- `void setValue (String value) `
  - Sets attribute value
- `Element getOwnerElement () ` - can be useful
  - Returns the element node of the attribute 
- `TypeInfo getSchemaTypeInfo () `
  - Returns information on attribute type 
- `boolean getSpecified () `
  - true = the attribute was explicitly specified in the document
- `boolean isId ()`
  - true = the attribute is of type ID

#### Interface CharacterData 

- `String getData ()` - get
  - Returns the text data 
- `int getLength () `
  - Returns the length of the data 
- `String substringData (int offset, int count) `
  - Returns the required substring of the data 
- `void setData (String data) ` - set
  - Sets the text data
- `void insertData (int offset, String arg) ` - insert
  - Inserts a part of the data at the specified index 
- `void appendData (String arg)` - append
  - Appends a new part of the data at the end
- `void deleteData (int offset, int count) `
  - Removes the specified part part of the data 
- `void replaceData (int offset, int count, String arg)`
  - Replaces the specified part of the data

#### Interface Text 

Methods of CharacterData plus:
- `String getWholeText () ` - get all text
  - Returns the text content of all logically neighbouring text child nodes connected into one
- `Text replaceWholeText (String content) `
  - Replaces textual content of all logically neighbouring text child nodes
- `boolean isElementContentWhitespace () ` - if it is valid text
  - true = the text node contains insignificant white spaces 
- `Text splitText (int offset)`
  - Splits the text at the given position into two

#### Interface ProcessingInstruction (PI) 

#### Interface Notation 

#### Interface Entity 

#### Interface DocumentType 

#### Other Interfaces
- Interface DocumentFragment 
  - Just methods of Node 
- Interface EntityReference 
  - Just methods of Node 
- Interface CDATASection 
  - Methods of Node, Text and CharacterData 
- Interface Comment
  - Methods of Node and CharacterData

#### Other classes

E.g. DOMLocator 
- DOM Level 3 
- Similar to SAX locator 
- Attributes: lineNumber, columnNumber, relatedNode, …
- One of properties of class DOMError 
  - Parameter of method handleError of class DOMErrorHandler which is a 
    - property of class DOMConfiguration which is a 
      - property of class Document (but from Level 3)

### JAXP

~ Java API for XML Processing

- https://jaxp.java.net/ 
- http://java.sun.com/j2ee/1.4/docs/tutorial/doc/ 


- JAXP 1.3 is a part of J2SE 5.0 
- JAXP 1.4 is a part of Java SE 6.0.
  - Corrected errors in JAXP 1.3 + support for StAX 
- Parsing, validation, transformation
  - XML 1.0, XML 1.1 
  - SAX 2.0.2 
  - StAX 1.0 
  - DOM Level 3 Core, DOM Level 3 Load and Save
  - XInclude 1.0, W3C XML Schema 1.0, XSLT 1.0, XPath 1.0 

---
## XML Databases

Motivation: requirements of applications
- Processing of external data
  - Web pages, other textual data, structured data 
- E-commerce 
  - Lists of goods, personalized views of the lists, orders, invoices, … 
- Integration of heterogeneous information resources 
  - Integrated processing of data from Web pages and from relational databases 

Main reason: storing XML data into databases means management of huge volumes of XML data in an efficient way

What we want: persistent storage of XML data
- General classification:
  - Based on a file system
  - Based on an object model
  - Based on (object-)relational databases 
    - XML-enabled databases 
    - Exploit a mapping method between XML data and relations 
  - Native XML databases 
    - Exploit a suitable data structure for hierarchical tree data
    - Usually a set of numbering schemas

The most efficient approaches are the native ones
- Reason: From the beginning they target the XML data structure
  - They are based on it 
- Disadvantage: We need to start from scratch 
  - The databases are not only about storing the data, but also transactions, versioning, multi-user access, replication, …


- An alternative intuitive idea: Exploitation of a mature and verified technology of (object-) relational databases


### Documents vs. Databases

| World of documents           | World of databases          |
| ---------------------------- | --------------------------- |
| many small documents         | several huge databases      |
| usually static               | usually dynamic             |
| implicit structure - tagging | explicit structure - schema |
| suitable for humans          | suitable for machines       |
|                              |                             |
| editing                      | updating                    |
| printing                     |                             |
| lexical checking             | data cleaning               |
| word count                   |                             |
| information retrieval        | querying                    |
| searching                    | storing/transforming        |

### Documents and Structured Data

- The border between the world of documents and world of databases is not exact 
  - In some proposals both kinds of access are possible 
  - Somewhere in the middle we can find formatting languages and semi-structured data 
- Semi-structured data are defined as data which are not sorted (have arbitrary order), which are not complete (have optional parts) and whose structure can "unpredictably" change 
  - Web data, HTML pages, Bibtex files, biological and chemical data
  - XML data are a kind of semi-structured data

### Classification of XML Documents

The basic classification of XML documents results from their origin and the way they were created
- data-oriented
- document-oriented 
- hybrid

For the particular classes different ways of implementations are suitable

#### Implementation Approaches

Differ according to the type of documents
- Exploit typical features
- Problem: hybrid documents
  - Ambiguous classification


- Document-oriented techniques
vs.
- Data-oriented techniques

#### Data-oriented XML Documents

- Usually created and processed by machines
- Regular, deep structure
  - Fully structured data 
- They do not contain
  - Mixed-content elements
  - CDATA sections
  - Comments 
  - Processing instructions
- The order of sibling elements is often unimportant 

Example: database exports, catalogues, …

```xml
<book id="12345">
  <title>All I Really Need To Know I Learned in Kindergarten</title> 
  <author>
    <name>Robert</name> 
    <surname>Fulghum</surname>
  </author>
  <edition title="Argo"> 
    <year>2003</year>
    <ISBN>80-7203-538-X</ISBN>
  </edition>
  <edition title="Argo">
    <year>1996</year> 
    <ISBN>80-7203-028-0</ISBN>
  </edition>
</book>
```

##### Techniques

Exploit data-oriented aspects (low level of round tripping)
- It is not necessary to preserve the document as a whole
  - Order of sibling elements is ignored, document-oriented constructs (comments, whitespaces, …) are ignored, …
- No (little) support for mixed-content elements
1. Middleware
    - A separate software which ensures transformation of XML data between XML documents and relations
2. XML-enabled database
    - RDBMS with functions and extensions for XML data support
3. Special related approach: XML data binding
    - Methods for binding of XML data and objects
    - For each element type a separate class
      - Its attributes and subelements form properties of the class
      - I.e. it is not a DOM tree of objects!
4. Mapping
    - Idea: The data are stored in a relational database management system (RDBMS) 
    - Mapping method – transforms the data into relations (and back) 
    - XML queries over XML data → SQL queries over relations
    - The result of SQL query → XML document

#### Document-oriented XML Documents

- Usually created and processed by humans 
- Irregular, less structured 
  - Semi-structured data 
- Often contain 
  - Mixed-content elements 
  - CDATA sections 
  - Comments 
  - Processing instructions 
- The order of sibling elements is crucial 

Example: XHTML web pages

```xml
<book id="12345">
  <title>All I Really Need To Know I Learned in Kindergarten</title> 
  <author>Robert Fulghum</author>
  <description>A new, edited and extended publication published on the occasion of the fifteen anniversary of the first edition</description> 
  <Text> <p>Fifteen years after publishing of <q>his</q> <i>Kindergarten</i> Robert Fulghum has decided to read it once again, now in <i>2003</i>.</p> <p>He wanted to find out whether and, if so, to what extent his opinions have changed and why. Finally, he modified and extended his book to...</p> 
  </Text>
</book>
```

##### Techniques

We need to preserve the document as whole
- Order of sibling elements
- Comments, CDATA sections, ...
- Even whitespaces
  - For legal documents

Round tripping – storing a document into a database and its retrieval 
- The level of round tripping says to what extent the documents are similar
  - The higher level, the higher similarity 
- In the optimal case they are equivalent

1. Native XML databases (NXD) 
    - Natural support for XML operations
      - XML query languages, XML update operations, DOM/SAX interfaces, … 
      - Focus on document-oriented aspects
        - Comments, CDATA sections, … 
    - The logical model is based on XML 
      - i.e. we work with trees 
    - The physical model can be, e.g., relational 
      - i.e. we can physically store the trees, e.g., into relations
    - (+) Good level of round tripping
    - (–) The index (numbering schema) is (used to be) several times bigger than the data, necessity to start from scratch (transactions, replication, multi-user access, query optimization, …)
2. LOB
    - Storing of the whole document into a BLOB (binary large object) / CLOB (character large object) column
      - Possible in all known database systems
    - (+) The highest level of round tripping, fast retrieval of the whole document, extending of XML data with database features
    - (–) No XML operations 
      - The data need to be extracted from the DB and pre-processed
3.  XML data type
    - Like a LOB with the support for XML operations 
      - XML querying, XML full-text search
      - Requires special indices (numbering schemas)
    - SQL/XML

### Numbering Schemas

A numbering schema of a tree model of a document is a function which assigns each node a unique identifier that serves as a reference to that node for indexing and query evaluation 

Enable fast evaluation of selected relationships among nodes of XML document
- Ancestor-descendant
- Parent-child
- Element-attribute
- …
- Depth of the node
- Order among siblings
- …

> Like database indexes

1. Sequential numbering schema
   - The identifiers are assigned to the nodes as soon as they are added to the system sequentially, starting from 1
2. Structural numbering schema
   - Enables to preserve and evaluate a selected relationship among any two nodes of the document
   - Often it is expected to enable fast searching for all occurrences of such a relationship in the document
3. Stable numbering schema
   - A schema which does not have to be modified (except for preserving its local features) when the structure of the respective data changes 
     - i.e., on insertion/deletion of nodes

A schema of a structural numbering schema
- Is an ordered pair `(p, L)`, where `p` is a binary predicate and `L` is an invertible function which for the given XML tree model `T = (N, E) `assigns each node `v ∈ N` a binary sequence `L(v)`.
- For each pair of nodes `u`, `v ∈ N` predicate `p(L(u), L(v))` is satisfied if `v` is in a particular relationship with `u`.
  - e.g. `v` is a descendant of u
- Particular numbering schema: particular `p` and `L`

#### Dietz Numbering

![53c43598.png](attachments/15e42996-b386-4d59-87af-840ece336296/53c43598.png)

- Preorder traversal
  - Child nodes of a node follow their parent node
- Postorder traversal
  - Parent node follows its child nodes
- Construction of a numbering schema
  - Each node `v ∈ N` is assigned with a pair `(x,y)` denoting preorder and postorder order
  - Node `v ∈ N` having `L(v) = (x,y)` is a descendant node of node u having `L(u) = (x',y')` if `x' < x & y' > y`

#### Depth-first (DF) Numbering

![f369273d.png](attachments/15e42996-b386-4d59-87af-840ece336296/f369273d.png)

can be non sequential because it doesnt cause whole tree to be recalculated on insert

#### ORDPATH

prefix and using odd numbers

![a118441e.png](attachments/15e42996-b386-4d59-87af-840ece336296/a118441e.png)

Insert 
- beginning using negative number
- end using next odd
- middle using even number with subnodes

![7c01ecec.png](attachments/15e42996-b386-4d59-87af-840ece336296/7c01ecec.png)

#### XML Databases - Mapping Methods

Methods for transformation between XML data and relations
Further classification:
1. Generic – mapping regardless XML schema of the stored XML data
2. Schema-driven – mapping based on XML schema of the stored XML data 
    - DTD, XML Schema
3. User-defined – mapping provided by the user

#### Generic Methods

Do not exploit XML schema of the stored data
Idea: Not all data have a schema 
Approaches: 
1. A relational schema for a particular type of (collection of) XML data 
    - e.g. Table-based mapping 
    - > Fixed.
2. A general relational schema for any type of (collection of) XML data 
    - View XML data as a general tree
      - We store the tree
    - e.g. Generic-tree mapping, Structure-centred mapping, Simple-path mapping
    - > General

##### Table-based Mapping

```xml
<Tables>
  <Table_1>
    <Row>
      <Column_1>...</Column_1> 
      ...
      <Column_n>...</Column_n>
    </Row>
    ... 
  </Table_1>
  ... 
  <Table_n> 
    <Row>
      <Column_1>...</Column_1>
      ...
      <Column_m>...</Column_m>
    </Row>
    ...
  </Table_n>
</Tables>
```

Trivial case
The schema is an implicit part of the data
- Only a limited set of documents can be stored 

Typical usage: (export) data transfer among multiple databases
There exist also more complex schemas, but the idea is the same
- Basically again usage of (an implicit) schema

##### Generic-tree Mapping 

The target relational schema enables to store any kind of XML data
- Regardless their XML schema

XML document <--> directed tree
- Inner nodes have an ID
- Leaves carry values of attributes or text nodes
- Outgoing edges of a node represent subelements/attributes of the element represented by ingoing edge of the same node
- Edges are labeled with element/attribute names

![70a7f90b.png](attachments/15e42996-b386-4d59-87af-840ece336296/70a7f90b.png)

- Edge mapping
  - `Edge (sourceID, order, label, type, targetID)`
  - Type: inner edge, element/attribute edge, …
  - `Edge (..., (1, 2, "name", element, -1), ... (1, 4, "address", inner, 2), ...)`


- Attribute mapping
  - Attribute = name of the edge >> groub by type on Edge mapping
  - `Edgeattribute (sourceID, order, type, targetID)`
  - `Edgename(..., (1, 2, element, -1), ... (3, 2, element, -1), ...)`


- Universal mapping
  - `Uni (sourceID, ordera1, typea1, targetIDa1, ... orderak, typeak, targetIDak)`
    - Outer join of tables from attribute mapping
    - `a1, ... ak` are all the attribute names in the XML document 
  - Too many null values 


- Normalized universal mapping 
  - The universal table contains for each name just one record 
  - Others (i.e. multi-value attributes) are stored in `overflow tables`
    - From edge mapping

How do we store the leaf values?
1. Special value tables, each for each data type used 
2. Value columns in the previous tables 
    - Many null values (for each data type an extra column)
    - Or we ignore data types
3. Other options 
    - Combination of previous approaches
    - E.g. attribute mapping for frequent attributes and edge mapping for other

##### Structure-centred Mapping

XML document <--> directed tree
- All nodes have the same structure: `N = (t, l, c, n)`, where
  - `t` is the type of node (i.e. ELEM, ATTR, TXT, ...)
  - `l` is the label of node (if exists) 
  - `c` is text content of node (if exists)
  - `n = {N1, ... Nm}` is (possibly empty) list of child nodes

Variants of the algorithm = variants of storing the list of child nodes
- Aim: efficient operations

Different way of storing the info about child nodes

1. Keys and foreign keys
    - Each node is assigned with an ID (key) and ID of its parent node (foreign key) 
    - (+) Simple, efficient updates 
    - (–) Inefficient queries (joins of many tables) 
2. DF values 
    - Node ID = pair `(DFmin, DFmax)`
      - `DFmin` = the time of visiting a node
      - `DFmax` = the time of leaving a node 
    - (+) Efficient querying and reconstruction of a node 
      - E.g. `v` is a descendant of `u`, if `umin < vmin` and `vmax < umax`
      - The nodes can be ordered totally
    - (–) Inefficient updates
      - In the worst case we need to re-number the whole tree
    - ![ca0a9989.png](attachments/15e42996-b386-4d59-87af-840ece336296/ca0a9989.png)
3. SICF (simple continued fraction) values
    - SICF node identifier = `sigma`, where `qi is element of N` (i = 1, ... k)
      - Sequence `<q1, ... qk>` identifies the node 
    - For root node: SICF ID `sigma = <s>`, s \> 1 
    - ![1163c3c3.png](attachments/15e42996-b386-4d59-87af-840ece336296/1163c3c3.png)
    - For all other nodes:
      If node `u` has `SICF ID = <q1, ... qm>` and `n` child nodes `u1, ... un`, then SICF ID of i-th child node is `<q1, ... qm, i>` 
      - Resembles to ORDPATH 
      - Does not have its advantages 
        - We do not use the “trick” with odd and even numbers 
    - (+) we have a more precise structural information
    - (–) like in the previous case

##### Simple-path Mapping

Assumption: XPath queries
Idea: We can store all paths to all nodes in the documents
- So-called simple paths

```c
<SimpleAbsolutePathUnit> ::= <PathOp> <SimplePathUnit> |
                             <PathOp> <SimplePathUnit> ’@’ <AttName>
<PathOp>                 ::= ’/’
<SimplePathUnit>         ::= <ElementType> |
                             <ElementType> <PathOp> <SimplePathUnit>
```

Just a simple path is not sufficient information
- It does not contain information about position/order of node in the document

Relational schema:
- Element (IDdoc, IDpath, Order, Position)
- Attribute (IDdoc, IDpath, Value, Position)
- Text (IDdoc, IDpath, Value, Position)
- Path (IDpath, Value)
  - Order of an element within its sibling nodes
  - Position of a word in a text is an integer value
  - Position of a tag is a real number 
    - integral part = position of the closest preceding word
    - decimal fraction = position within tags following the closest preceding word

(+) Efficient processing of XPath queries
- Implementation of ‘//’ using SQL LIKE

#### Schema-driven Mapping

Based on existence of an XML schema 
- Usually DTD or XML Schema 

- Algorithm:
  1. XML schema is mapped to relational schema
  2. XML data valid against the XML schema are stored into relations
      - i.e., for data with different structure (XML schema) we have a different relational schema
- Aim: We want to create an optimal schema with "reasonable" amount of tables and null values and which corresponds to the source XML schema

General characteristics of the algorithms:
1. For each element we create a relation consisting of its attributes
2. Subelements with maximum occurrence of one are (instead of to separate tables) mapped to tables of parent elements
    - so-called inlining
3. Elements with optional occurrence → nullable columns
4. Subelements with multiple-occurrence → separate tables 
    - Element-subelement relationships are mapped using keys and foreign keys
5. Alternative subelements →
    - separate tables (analogous to the previous case) or
    - one universal table (with many nullable fields)
6. Order of sibling elements (if necessary) → special column 
7. Mixed-content elements usually not supported
    - Would require many columns with nullable fields
8. Despite the previous optimizations a reconstruction of an element requires joining several tables. 


- Most of the techniques use an auxiliary graph
- Classification:
  - Fixed methods – exploit information only from schema
    - Basic, Shared and Hybrid
  - Flexible methods – exploit other information
    - LegoDB mapping, Hybrid object-relational mapping 

##### Fixed methods

###### Basic, Shared and Hybrid

- Continuous improvements of mapping a DTD to relational schema
  - One of the first approaches 
- DTD graph – auxiliary structure for creation of a relational schema
  - Nodes = elements (occur 1x) / attributes / operators
  - Directed edges = relationships element-subelement / element-attribute / element-operator / operator-element 

- Note: DTD is first "flattened" and simplified
  - Contains only operators `*` and `?` `(+ → *, a|b → a?,b?)`
  - A classical trick

![ed743f6d.png](attachments/15e42996-b386-4d59-87af-840ece336296/ed743f6d.png)

![57bba719.png](attachments/15e42996-b386-4d59-87af-840ece336296/57bba719.png)
![6e9d18e7.png](attachments/15e42996-b386-4d59-87af-840ece336296/6e9d18e7.png)
![c0c518a9.png](attachments/15e42996-b386-4d59-87af-840ece336296/c0c518a9.png)

##### Flexible methods

###### LegoDB mapping

![3f400e7a.png](attachments/15e42996-b386-4d59-87af-840ece336296/3f400e7a.png)
![df11e2e2.png](attachments/15e42996-b386-4d59-87af-840ece336296/df11e2e2.png)
![64b02a28.png](attachments/15e42996-b386-4d59-87af-840ece336296/64b02a28.png)

###### Hybrid object-relational mapping 

![5370c9ee.png](attachments/15e42996-b386-4d59-87af-840ece336296/5370c9ee.png)
![8d6e9cb5.png](attachments/15e42996-b386-4d59-87af-840ece336296/8d6e9cb5.png)
![8b57481d.png](attachments/15e42996-b386-4d59-87af-840ece336296/8b57481d.png)

#### User-defined Mapping

The whole mapping process is defined by the user 

Algorithm:
1. The user creates the target relational schema
2. The user specifies the required mapping (using a systemdependent interface)
    - Usually a declarative interface, annotations in XML schemas, special query languages, ...

- (+) The most flexible approach
  - The user knows what (s)he wants

- (–) The user must know several advanced technologies, the definition of an optimal relational schema is not an easy task

An attempt to solve the disadvantages of userdefined mapping
Idea: an implicit method + user-defined local changes
  - Annotation of schema = user denotes fragments (subtrees) whose storage strategy should be modified
  - Pre-defined set of allowed changes of mapping
    - Usually a set of attributes and their values

##### Example – system XCacheDB

- `INLINE` – inline the fragment into parent table
- `TABLE` – store the fragment into a separate table
- `BLOB_ONLY` – store the fragment into a BLOB column
- `STORE_BLOB` – store the fragment implicitly + into a BLOB column
- `RENAME` – change the name of table of column
- `DATATYPE` – change the data type of the column

### Current State of the Art of XML Databases

Native databases vs. XML-enabled databases
- The difference is fading away

- Oracle DB, IBM DB2, MS SQL Server – the storage is defined by the user
  - BLOB
  - Native XML storage (typically parsed XML data + ORDPATH numbering schema)
  - Decomposition into relations – fixed schema-driven or userdriven
    - Currently user-driven annotations often denoted as obsolete 


- Standard bridge between XML and relational world: SQL/XML

---