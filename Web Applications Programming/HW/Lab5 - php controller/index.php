<?php

main();

function main()
{
    // Try get valid page param value.
    $page = safeGetPage();
    if (!isset($page))
    {
        http_response_code(400);
        return;
    }

    // Try find template path.
    $path = safeGetIndexTemplatePath($page);
    $path = !isset($path) ? safeGetNamedTemplatePath($page) : $path;
    if (!isset($path))
    {
        http_response_code(404);
        return;
    }

    setParametersAndGenerate($path);
}

function safeGetPage()
{
    $page = safeGet($_GET, 'page');
    $page = isset($page) ? validateComposition($page) : NULL;
    return $page;
}

function safeGet(array $array, string $name, $default = null)
{
    if (!array_key_exists($name, $array)) {
        return $default;
    }
    return $array[$name];
}

function validateComposition($value)
{
    $matchResult = preg_match('/^([a-zA-Z]+)(\/[a-zA-Z]+)*$/', $value);
    return $matchResult ? $value : NULL;
}

function safeGetIndexTemplatePath(string $page)
{
    return safeGetTemplatePath("$page/index.php");
}

function safeGetNamedTemplatePath(string $page)
{
    return safeGetTemplatePath("$page.php");
}

function safeGetTemplatePath($suffix)
{
    $path = __DIR__ . "/templates/$suffix";
    return file_exists($path) ? "$suffix" : NULL;
}

function setParametersAndGenerate(string $relativePath)
{
    $path = __DIR__ . "/parameters/$relativePath";
    $parameters = file_exists($path) ? require $path : [];
    foreach ($parameters as $key => $param)
    {
        $value = safeGet($_GET, $key);
        $checkResult = checkParameterValue($param, $value);
        if (!$checkResult)
        {
            http_response_code(400);
            return;
        }

        if ($param == 'int')
        {
            $value = (int)$value;
        }

        $$key = $value;
    }

    require __DIR__ . "/templates/_header.php";
    require __DIR__ . "/templates/$relativePath";
    require __DIR__ . "/templates/_footer.php";
}

function checkParameterValue($param, $value)
{
    if (!isset($value))
    {
        return false;
    }

    if (is_array($param))
    {
        return in_array($value, $param);
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
