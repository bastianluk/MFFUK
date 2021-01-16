<?php

require_once(__DIR__ . "/lib.php");

function getMethod() : string
{
    return $_SERVER["REQUEST_METHOD"];
}

function getHomeUrl() : string
{
    return getPhpSelf() . "?page=home/index";
}

function getPhpSelf() : string
{
    return $_SERVER["PHP_SELF"];
}

function safeGet(array $array, string $name, $default = null)
{
    if (!array_key_exists($name, $array)) {
        return $default;
    }
    return $array[$name];
}

function getPage($parameters)
{
    $page = safeGet($parameters, 'page');
    $page = isset($page) ? validateComposition($page) : NULL;
    return $page;
}

function validateComposition($value)
{
    $matchResult = preg_match('/^([a-zA-Z]+)(\/[a-zA-Z]+)*$/', $value);
    return $matchResult ? $value : NULL;
}

function getPath(string $page)
{
    $path = getIndexTemplatePath($page);
    $path = !isset($path) ? getNamedTemplatePath($page) : $path;

    return $path;
}

function getIndexTemplatePath(string $page)
{
    return getTemplatePath("$page/index.php");
}

function getNamedTemplatePath(string $page)
{
    return getTemplatePath("$page.php");
}

function getTemplatePath($suffix)
{
    $path = __DIR__ . "/../templates/$suffix";
    return file_exists($path) ? "$suffix" : NULL;
}

function checkParameterValue($param, $value)
{
    if (!isset($value))
    {
        return false;
    }

    switch ($param) {
        case 'int':
            return is_numeric($value);
        case 'string':
            return is_string($value);
        default:
            return false;
    }
}