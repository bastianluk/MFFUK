<?php


class PathControllerAction
{
    public $path;

    public $controller;

    public $action;

    public function __construct($path, $controller, $action)
    {
        $this->path = $path;
        $this->controller = $controller;
        $this->action = $action;
    }
}
class Router
{
    public function dispatch()
    {
        /*
         * Your code goes here...
        */

        $arguments = [];
        $actionParameter = NULL;
        $functionPrefix = NULL;
        $method = $_SERVER["REQUEST_METHOD"];
        switch ($method)
        {
            case "GET":
                $functionPrefix = "get";
                $arguments = $_GET;
                $actionParameter = $this->safeGetAction($arguments);
                break;
            case "POST":
                $functionPrefix = "post";
                $actionParameter = $this->safeGetAction($_GET);
                $arguments = $_POST;
                break;
            default:
                throw new Exception("Problem");
        }

        if (!isset($actionParameter))
        {
            http_response_code(400);
            return;
        }

        $validationResult = $this->validateAction($functionPrefix, $actionParameter);
        if (!isset($validationResult)) {
            http_response_code(404);
            return;
        }

        require "$validationResult->path";

        if (!class_exists($validationResult->controller))
        {
            http_response_code(404);
            return;
        }
        $controller = new $validationResult->controller();


        $functionName = $validationResult->action;
        if (!method_exists($controller, $functionName))
        {
            http_response_code(404);
            return;
        }
        $reflectionMethod = new ReflectionMethod($controller, $functionName);
        $reflectionParameters = $reflectionMethod->getParameters();
        $parameterNames = [];
        foreach ($reflectionParameters as $parameter)
        {
            $parameterNames[] = $parameter->getName();
        }

        $ready_parameters = [];
        foreach ($parameterNames as $parameterName)
        {
            $value = $this->safeGet($arguments, $parameterName);
            if (!isset($value))
            {
                http_response_code(400);
                return;
            }
            $ready_parameters[$parameterName] = $value;
        }

        try
        {
            $callResult = call_user_func_array("$validationResult->controller::$functionName", $ready_parameters);
            if (!isset($callResult))
            {
                http_response_code(204);
                return;
            }

            echo json_encode($callResult);
        }
        catch (Exception $e)
        {
            http_response_code(500);
            echo $e->getMessage();
            return;
        }

    }
    private function safeGetAction($arguments)
    {
        $action = $this->safeGet($arguments, "action");
        return isset($action) && $this->validateComposition($action) ? $action : NULL;
        return $action;
    }
    private function safeGet(array $array, string $name, $default = null)
    {
        if (!array_key_exists($name, $array)) {
            return $default;
        }
        return $array[$name];
    }
    private function validateAction($preffix, $actionParameter)
    {
        preg_match("/^((([a-zA-Z\_]+)\/)+?)([a-zA-Z_]+)+$/", $actionParameter, $matches);
        $suffix = rtrim($matches[1], "/");
        $path = __DIR__ . "/controllers/$suffix.php";
        $fileExists = file_exists($path);
        if (!$fileExists)
        {
            return NULL;
        }

        return new PathControllerAction($path, $this->getControllerName($matches), $this->getFunctionName($preffix, $matches));
    }
    private function validateComposition($action)
    {
        return preg_match("/^((([a-zA-Z\_]+)\/)+?)([a-zA-Z_]+)+$/", $action, $matches);
    }
    private function getControllerName($matches)
    {
        return $matches[3] . "Controller";
    }
    private function getFunctionName($preffix, $matches)
    {
        return $preffix . $matches[4];
    }
}
