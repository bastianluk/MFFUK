<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");

//validateCall();

require_once(__DIR__ . "/../../entities/listItem.php");

function get_data()
{
    require_once(__DIR__ . "/../../lib/sql_lib.php");

    $context = new SqlContext();
    $items = $context->getAllListItems();

    usort($items, "compareListItems");

    return $items;
}

function compareListItems(ListItem $itemA, ListItem $itemB)
{
    return intcmp($itemA->position, $itemB->position);
}

function intcmp($intA, $intB)
{
    return (int)$intA < (int)$intB ? -1 : ((int)$intA == (int)$intB ? 0 : 1);
}