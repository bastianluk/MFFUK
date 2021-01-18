<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");
// validateCall();

massDelete($ids);

function massDelete($ids)
{
    require_once(__DIR__ . "/../../lib/lib.php");
    checkedBadRequest(parametersValid($ids));

    require_once(__DIR__ . "/../../data/sqlContext.php");
    $context = new SqlContext();
    $context->deleteListItemsByIds($ids);
}

function parametersValid($ids)
{
    return isArrayOfIds($ids);
}