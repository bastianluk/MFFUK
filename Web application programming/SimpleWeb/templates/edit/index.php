<?php

//require_once(__DIR__ . "/../../lib/subpage_lib.php");
//validateCall();

edit($id, $amount);

function edit($id, $amount)
{
    require_once(__DIR__ . "/../../lib/lib.php");
    checkedNotFound(parametersValid($id, $amount));

    require_once(__DIR__ . "/../../lib/sql_lib.php");
    $context = new SqlContext();
    $context->setAmount($id, $amount);
}


function parametersValid($id, $amount)
{
    return isIdValid($id) && isAmountValid($amount);
}
function isIdValid($id)
{
    return isset($id) && is_numeric($id);
}
function isAmountValid($amount)
{
    return isset($amount) && (
        is_numeric($amount) && $amount > 0
    );
}