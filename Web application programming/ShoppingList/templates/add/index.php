<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");
//validateCall();

add($name, $amount);

function add($name, $amount)
{
    require_once(__DIR__ . "/../../lib/lib.php");
    checkedBadRequest(parametersValid($name, $amount));

    require_once(__DIR__ . "/../../data/sqlContext.php");
    $context = new SqlContext();
    $context->upsertItemToList($name, $amount);
}

function parametersValid($name, $amount)
{
    return isNameValid($name) && isAmountValid($amount);
}