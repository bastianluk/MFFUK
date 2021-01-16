<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");

//validateCall();

function get_data()
{
    require_once(__DIR__ . "/../../lib/sql_lib.php");
    require_once(__DIR__ . "/../../entities/listItem.php");

    // This is rather ugly...
    $context = new SqlContext();
    $items = $context::getAllListItems();
    usort($items, "compareListItems");

    foreach ($items as $item)
    {
        echo "Item: $item->name";
    }

    return $items;
}

function compareListItems(ListItem $itemA, ListItem $itemB)
{
    return intcmp($itemA->position, $itemB->position);
}

function intcmp($intA, $intB)
{
    return (int)$a < (int)$b ? -1 : ((int)$a == (int)$b ? 0 : 1);
}