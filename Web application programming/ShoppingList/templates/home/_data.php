<?php

//require_once(__DIR__ . "/../../lib/subpage_lib.php");
//validateCall();

function get_listItems() : array
{
    require_once(__DIR__ . "/../../data/sqlContext.php");
    $context = new SqlContext();
    $listItems = $context->getAllListItems();

    usort($listItems, "compareListItems");

    return $listItems;
}

function compareListItems(ListItem $listItemA, ListItem $listItemB)
{
    return intcmp($listItemA->position, $listItemB->position);
}

function intcmp($intA, $intB)
{
    return (int)$intA < (int)$intB ? -1 : ((int)$intA == (int)$intB ? 0 : 1);
}