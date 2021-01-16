<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");

validateCall();

require_once(__DIR__ . "/../../entities/listItem.php");
require_once(__DIR__ . "/../../lib/sql_lib.php");

add();

function add()
{
    if (isNameInvalid($name) || isAmountInvalid($amount))
    {
        # Invalid request data
    }


    $item = findExistingItem($name);
    if (!isset($item))
    {
        // Create new + update the amount, add to datalist
    }
    else
    {
        // Update the amount
    }
}



function findExistingItem()
{
    $sqlContext = new SqlContext();
    // find by name
}

function isNameInvalid($name)
{
    return !isset($name) || !is_string($name);
}

function isAmountInvalid($amount)
{
    return !isset($amount) || !(
        is_numeric($amount) && $amount > 0
    );
}

function revalidate()
{
    if (
        (!isset($name) || !is_string($name)) ||
        (!isset($amount) || !(
            is_numeric($amount) && $amount > 0
        ))
    )
    {
        die("Invalid data");
    }
}