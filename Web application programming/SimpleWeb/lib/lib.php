<?php

function checkedBadRequest($condition)
{
    if (!$condition)
    {
        throw new Exception("Bad request - incorrect parameters");
    }
}

function checkedNotFound($condition)
{
    if (!$condition)
    {
        throw new Exception("Requested page not found");
    }
}

function checkedRedirectOther($condition, $redirect)
{
    if (!$condition)
    {
        redirectOther($redirect);
    }
}

function redirectOther($redirect)
{
    header( "Location: {$redirect}", true, 303 );
    exit();
}

//function checkedAction(bool $condition, $function, array $parameters)