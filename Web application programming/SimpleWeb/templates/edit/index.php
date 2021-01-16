<?php

// require_once(__DIR__ . "/../../lib/subpage_lib.php");

//validateCall();

edit($id, $amount);

function edit($id, $amount)
{
    require_once(__DIR__ . "/../../lib/sql_lib.php");

    $context = new SqlContext();
    $context->setAmount($id, $amount);
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