<?php

require_once(__DIR__ . "/lib.php");

function validateCall()
{
    checkedNotFound(!defined('rudamentaryValidation')); // Should be 403, but that gives away the fact that the page exists at all.
}