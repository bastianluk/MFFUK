<?php

require_once(__DIR__ . "/lib/index_lib.php");

try
{
    main();
}
catch (Exception $exception)
{
    $homeUrl = getHomeUrl(); // Lets hope there is nothing wrong in the home page code :)
    $encodedMessage = urlencode("Exception occured: " . $exception->getMessage());
    $newUrl = $homeUrl . "&message=$encodedMessage";
    redirectOther($newUrl);
}

function main()
{
    //define('rudamentaryValidation', 322); // to prevent direct access to templates

    $method = getMethod();
    $page = getPage($_GET);
    $fullPath = getPath($page);
    $parameters = $method == 'POST' ? $_POST : $_GET;
    processRequest($method, $fullPath, $parameters);
}

function getPage($parameters) : string
{
    $page = getPageValue($parameters);
    checkedBadRequest(isset($page));

    return $page;
}

function getPath($page)
{
    $fullPath = getPathValue($page);
    checkedNotFound(isset($fullPath));

    return $fullPath;
}

function processRequest(string $method, $relativePath, array $parameters)
{
    $parametersFullPath = __DIR__ . "/parameters/$relativePath";
    $templatePrefix =__DIR__ . "/templates";
    $templateFullPath = $templatePrefix . "/$relativePath";
    $requiredParameters = file_exists($parametersFullPath) ? require $parametersFullPath : [];
    foreach ($requiredParameters as $key => $param)
    {
        $value = safeGet($parameters, $key);
        $checkResult = isCorrectlyTyped($param, $value);
        checkedBadRequest($checkResult);
        if ($param == 'int')
        {
            $value = (int)$value;
        }
        if (is_array($param))
        {
            $exploded = explode(",", $value);
            $mapped = array_map(function($item) { return (int)$item ;}, $exploded);
            $value = $mapped;
        }

        $$key = $value;
    }

    $isPost = $method == 'POST';
    if ($isPost)
    {
        require $templateFullPath;
        $redirect = safeGet($parameters, "redirectOnPost");
        if (isset($redirect))
        {
            redirectOther(getHomeUrl());
        }
    }
    else
    {
        require $templatePrefix . "/_header.php";
        $message = safeGet($parameters, "message");
        if (isset($message))
        {
            $decodedMessage = urldecode($message);
            require $templatePrefix . "/_message.php";
        }
        require $templateFullPath;
        require $templatePrefix . "/_footer.php";
    }
}