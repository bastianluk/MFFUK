<?php

function checkedBadRequest($condition)
{
    if (!$condition)
    {
        http_response_code(400);
        exit();
    }
}

function checkedNotFound($condition)
{
    if (!$condition)
    {
        http_response_code(404);
        exit();
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