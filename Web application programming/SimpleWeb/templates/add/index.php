<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");

validateCall();

require_once(__DIR__ . "/../../entities/listItem.php");
require_once(__DIR__ . "/../../lib/sql_lib.php");

add();

function add()
{
    checkedBadRequest(parameterValid($name, $amount));

    upsertItemToList($name, $amount);
}

function parametersValid($name, $amount)
{
    return isNameValid($name) && isAmountValid($amount);
}

function isNameValid($name)
{
    return isset($name) && is_string($name);
}

function isAmountValid($amount)
{
    return isset($amount) && (
        is_numeric($amount) && $amount > 0
    );
}