# Preparation for Programming Test

The goal is to develop a simple web application with PHP and JavaScript that allows the user to edit a shopping list and includes an autocomplete feature.
The shopping list consists of an ordered set of items (i.e., an item cannot occur multiple times in the list and the order of items matters).
Each item is associated with an amount value (an integer specifying quantity). The list supports the following operations:

- Insertion of a new item by submitting a form under the shopping list (filled in with the name of the item and desired amount/quantity).
- Deletion of items from the list.
- Editing the amount associated with each item.
- Modification of the order of the items on the list.

When a new item is being added to the list, the form automatically suggests names of items that have been used before (autocomplete feature).
It is sufficient to appropriately use the `<datalist>` element for this purpose, but you can implement this feature in JavaScript as well.
The form for adding a new item should be submitted normally -- i.e., without Javascript -- and handled by a PHP script.
The user is allowed to add an item with name that has never been used before (in that case, the new item is added to the list of known items).

Implement deletion of items with AJAX. Clicking a "delete" button by the respective item initiates an asynchronous HTTP request which deletes the item from the database.
JavaScript should remove the item from the HTML page upon successful deletion from the database.

The user can change the amount value associated with each item after (s)he clicks an "edit" button next to the item.
A simple form with a single editable item and save and cancel buttons should be displayed. The form can be displayed inline in the list, or as an independent dialog box.

You are free to choose how to implement reordering of items. One of the simplest solutions is adding buttons that allow to swap two neighbouring items. Another option is using the drag-n-drop technique.

You are not allowed to use any third-party libraries or code snippets (in JavaScript nor PHP).

Please store the database access configuration in a standalone file called `db_config.php` created from the following template. This file is not considered a part of the solution (do not submit it anywhere).

```
<?php
  $db_config = array(
    'server'   => 'localhost',
    'login'    => '<my-username>',
    'password' => '<my-password>', // does not need to be submitted with the project
    'database' => '<my-username>',
   );
```

## Database Schema

Data shall be stored in a MySQL database. You can use a prepared [database schema](https://webik.ms.mff.cuni.cz/semestralwork/db_sample.sql). If you execute statements in the file, two tables will be created:

- `items` -- list of names of all items that were ever used in the list
- `list` -- the current contents of the list

The `list` does not contain item names directly, but references the `items` table through a foreign key.
When adding a new item (entered by the user as a string in the add-item form), search the `items` table first for a matching name. If the user entered an item name that is not present in the `items` table, it has to be added.

## Grading Details

The absolute criterium is functionality. Your semestral work must implement all the aforementioned features as described, otherwise it will not be accepted.
You can diverge from the specification slightly if the intended scope of tested knowledge and skills to be demonstrated by the work is maintained.

Make sure your implementation fulfills the following conditions:

- Valid HTML and CSS.
- Reasonable architecture of the entire application using the front-controller and possibly MVC/MVP patterns. HTML and PHP should be reasonably separated (using some form of templating).
- Systematically handle insertion of values to HTML output (templates) to prevent script-injection attacks.
- Systematically handle querying of the database to prevent SQL injection. Use reasonable abstractions for the data layer.
- All user inputs and data passed in the URL should be validated or appropriately sanitized.
- At least one HTML form (e.g., for the insertion of a new item) should be submitted and handled in a standard way (submitted as a standard form, handled by the built-in PHP mechanisms). Furthermore, you need to correctly prevent form re-submission, when the user refreshes the page or uses back-forward movement in the browser history.
- At least one action (e.g., deletion of a list item) should be performed without reloading the page (i.e., using AJAX). JavaScript handler should delete the item from HTML upon deletion confirmation from the server.
- The used forms appropriately validate inputs using HTML5 validation attributes.
- The requests to server use appropriate HTTP methods. Use `GET` only to obtain data. Operations modifying the data should use `POST` or another appropriate method (e.g., `DELETE`).
- Errors are displayed to the user in a sensible way.

## Example Implementation

![Main Page](https://webik.ms.mff.cuni.cz/semestralwork/screenshot1.png)

![Adding New Item with Suggestions](https://webik.ms.mff.cuni.cz/semestralwork/screenshot2.png)

![Editting Amount](https://webik.ms.mff.cuni.cz/semestralwork/screenshot3.png)