<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");
// validateCall();

move($id, $oldPosition, $newPosition);

function move($id, $oldPosition, $newPosition)
{
    require_once(__DIR__ . "/../../lib/lib.php");
    checkedBadRequest(parametersValid($id, $oldPosition, $newPosition));

    require_once(__DIR__ . "/../../data/sqlContext.php");
    $context = new SqlContext();
    $context->moveListItemTo($id, $oldPosition, $newPosition);
}

function parametersValid($id, $oldPosition, $newPosition)
{
    return isIdValid($id) && isPositionValid($oldPosition) && isPositionValid($newPosition);
}