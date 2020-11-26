<?php

/*
 * Your code goes here...
 */

// Copy paste from Lab solutions on Webik
// Possibly extract to a separate file
function loadCsv($filePath)
{
    $result = [];
    if ($file = fopen($filePath, 'r')) {
        while (($data = fgetcsv($file))) { // the assignment is intentional (it is not a comparison)
            $result[$data[0]] = $data; // first cell is ISBN, lets use it as a key
        }
    }
    return $result;
}

function addHeader()
{
    require __DIR__ . '/templates/_header.php';
}

function addFooter()
{
    require __DIR__ . '/templates/_footer.php';
}

// This is both white list for existing pages and list of titles.
$titles = [
    "index"   => "Welcome",
    "cart"    => "Cart",
    "catalog" => "Catalog",
];

// Safely get the query parameter 'page' ...
$page = isset($_GET['page']) ? $_GET['page'] : "index";

if (array_key_exists($page, $titles)) { // isset would be also fine
    // Global variable $title will be available also in included files...
    $title = $titles[$page];

    // This will dump the common prefix (header.php will use $title)
    addHeader();

    // Data handling should not be in templates (so we will do it here).
    if ($page == 'catalog') {
        $products = loadCsv('./products.csv');
    }

    // Print out the selected contents of the page.
    require __DIR__ . "/templates/$page.php";

    // And finally, print out common suffix (footer) of all pages.
    addFooter();
} else {
    // Invalid page parameter - print out error message.
    http_response_code(408);
    $title = 'Error';
    addHeader();
    echo "Page does not exist";
    addFooter();
}