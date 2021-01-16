<?php

function safeGet(array $array, string $name, $default = null)
{
    if (!array_key_exists($name, $array)) {
        return $default;
    }
    return $array[$name];
}