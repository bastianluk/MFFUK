<?php

function checkedBadRequest($condtion)
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
        header( "Location: {$redirect}", true, 303 );
        exit();
    }
}

//function checkedAction(bool $condtion, $function, array $parameters)