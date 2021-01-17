<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");
// validateCall();

delete($id, $position);

function delete($id, $position)
{
    require_once(__DIR__ . "/../../lib/lib.php");
    checkedNotFound(parametersValid($id, $position));

    require_once(__DIR__ . "/../../lib/sql_lib.php");
    $context = new SqlContext();
    $context->deleteListItemAt($id, $position);
}

function parametersValid($id, $position)
{
    return isIdValid($id) && isPositionValid($position);
}