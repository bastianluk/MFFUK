<?php

require_once(__DIR__ . "/lib/index_lib.php");

main();

function main()
{
    define('rudamentaryValidation', 42); // to prevent direct access to templates

    $method = getMethod();
    $page = getPageOrRedirectHome($_GET);
    $fullPath = getPathOrNotFound($page);
    $parameters = $method == 'POST' ? $_POST : $_GET;
    processRequest($method, $fullPath, $parameters);
}

function getPageOrRedirectHome($parameters) : string
{
    $page = getPage($parameters);
    checkedRedirectOther(isset($page), getHomeUrl());

    return $page;
}

function getPathOrNotFound($page)
{
    $fullPath = getPath($page);
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
        $checkResult = checkParameterValue($param, $value);
        checkedBadRequest($checkResult);

        if ($param == 'int')
        {
            $value = (int)$value;
        }

        $$key = $value;
    }

    $isPost = $method == 'POST';
    if ($isPost)
    {
        require $templateFullPath;
        redirectOther(getHomeUrl());
    }
    else
    {
        require $templatePrefix . "/_header.php";
        require $templateFullPath;
        require $templatePrefix . "/_footer.php";
    }
}
