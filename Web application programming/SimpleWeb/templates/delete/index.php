<?php

// require_once(__DIR__ . "/../../lib/subpage_lib.php");

// validateCall();

delete($id, $position);

function delete(int $id, int $position)
{
    require_once(__DIR__ . "/../../lib/sql_lib.php");

    $context = new SqlContext();
    $context->deleteListItemAt($id, $position);
}

function parametersValid()
{
    return true;
}