<?php

main();

function main()
{
    $method = getMethod();
    if ($method == 'POST')
    {
        // Execute code (such as database updates) here.

        // Redirect to this page.
        header( "Location: {$_SERVER['REQUEST_URI']}", true, 303 );
        exit();
    }
    if ($_POST) {

     }
}

function getMethod()
{
    return $_SERVER["REQUEST_METHOD"];
}

function postWithSafeRedirect($parameters)
{

}