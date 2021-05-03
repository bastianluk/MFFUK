<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");
//validateCall();

edit($id, $amount);

function edit($id, $amount)
{
    require_once(__DIR__ . "/../../lib/lib.php");
    checkedBadRequest(parametersValid($id, $amount));

    require_once(__DIR__ . "/../../data/sqlContext.php");
    $context = new SqlContext();
    $context->setAmount($id, $amount);
}

function parametersValid($id, $amount)
{
    return isIdValid($id) && isAmountValid($amount);
}