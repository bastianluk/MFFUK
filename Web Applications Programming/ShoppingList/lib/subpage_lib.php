<?php

require_once(__DIR__ . "/lib.php");

function validateCall()
{
    checkedNotFound(!defined('rudamentaryValidation')); // Should be 403, but that gives away the fact that the page exists at all.
}

function isNameValid($name)
{
    return isset($name) && is_string($name);
}

function isIdValid($id)
{
    return isset($id) && is_numeric($id);
}

function isPositionValid($position)
{
    require_once(__DIR__ . "/../data/sqlContext.php");
    $context = new SqlContext();
    $maxPosition = $context->getMaxListPosition();
    $maxAllowedPosition = $maxPosition == 0 ? 1 : $maxPosition;

    return isset($position) && (
        is_numeric($position) && 0 < $position && $position <= $maxAllowedPosition
    );
}

function isAmountValid($amount)
{
    return isset($amount) && (
        is_numeric($amount) && $amount > 0
    );
}

function isArrayOfIds($ids)
{
    return (
        is_array($ids) &&
        array_reduce(
            $ids,
            function ($carry, $id) {
                return $carry && isIdValid($id);
            },
            true
        )
    );
}