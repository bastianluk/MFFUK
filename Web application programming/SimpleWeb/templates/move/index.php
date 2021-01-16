<?php

// require_once(__DIR__ . "/../../lib/subpage_lib.php");

// validateCall();

move($id, $oldPosition, $newPosition);

function move(int $id, int $oldPosition, int $newPosition)
{
    require_once(__DIR__ . "/../../lib/sql_lib.php");

    $context = new SqlContext();
    $context->moveListItemTo($id, $oldPosition, $newPosition);
}

function parametersValid()
{
    return true;
}