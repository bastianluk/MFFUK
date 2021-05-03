<?php

require_once(__DIR__ . "/recodex_lib.php");
main();

class ValidationResult
{
    public $value;

    public $validFields;

    public $invalidFieldNames;

    public function __construct($value, $validFields, $invalidFieldNames)
    {
        $this->value = $value;
        $this->validFields = $validFields;
        $this->invalidFieldNames = $invalidFieldNames;
    }
}

class ValidationStructure
{
    public $formFieldName;

    public $shouldGet;

    public $validationFunction;

    public $valueGetter;

    public $isRequired;

    public function __construct($formFieldName, $shouldGet, $validationFunction, $valueGetter, $isRequired)
    {
        $this->formFieldName = $formFieldName;
        $this->shouldGet = $shouldGet;
        $this->validationFunction = $validationFunction;
        $this->valueGetter = $valueGetter;
        $this->isRequired = $isRequired;
    }
}

function main()
{
    if ($_SERVER["REQUEST_METHOD"] == "POST")
    {
        $validationResult = validate($_POST);
        if ($validationResult->value)
        {
            $fields = $validationResult->validFields;

            $fromTime = array_key_exists("fromTime", $fields) ? (int)$fields["fromTime"] : NULL;
            $toTime = array_key_exists("toTime", $fields) ? (int)$fields["toTime"] : NULL;
            $gifts = array_key_exists("gifts", $fields) ? $fields["gifts"] : [];
            $giftCustom = array_key_exists("giftCustom", $fields) ? $fields["giftCustom"] : NULL;

            recodex_save_survey(
                $fields["firstName"],
                $fields["lastName"],
                $fields["email"],
                $fields["deliveryBoy"],
                (int)$fields["unboxDay"],
                $fromTime,
                $toTime,
                $gifts,
                $giftCustom
            );
        }
        else
        {
            recodex_survey_error("Invalid input", $validationResult->invalidFieldNames);
        }

        header("Location: index.php", true, 302);
    }
    else
    {
        require __DIR__ . "/form_template.html";
    }
}

function validate(array $arguments) : ValidationResult
{
    $deliveryBoys = ["jesus", "santa", "moroz", "hogfather", "czpost", "fedex"];
    $days = ["24", "25"];
    $formFields = [
        new ValidationStructure(
            "firstName",
            function() {
                return true;
            },
            function($value, $shouldGet) {
                return is_nonEmptyString($value) && strlen($value) <= 100;
            },
            function($value, $bool) {
                return $value;
            },
            true
        ),
        new ValidationStructure(
            "lastName",
            function() {
                return true;
            },
            function($value, $shouldGet) {
                return is_nonEmptyString($value) && strlen($value) <= 100;
            },
            function($value, $bool) {
                return $value;
            },
            true
        ),
        new ValidationStructure(
            "email",
            function() {
                return true;
            },
            function($value, $shouldGet) {
                return is_nonEmptyString($value) && filter_var($value, FILTER_VALIDATE_EMAIL) && strlen($value) <= 200;
            },
            function($value, $bool) {
                return $value;
            },
            true
        ),
        new ValidationStructure(
            "deliveryBoy",
            function() {
                return true;
            },
            function($value, $shouldGet) use ($deliveryBoys) {
                return in_array($value, $deliveryBoys);
            },
            function($value, $bool) {
                return $bool ? $value : NULL;
            },
            true
        ),
        new ValidationStructure(
            "unboxDay",
            function() {
                return true;
            },
            function($value, $shouldGet) use ($days) {
                return in_array($value, $days);
            },
            function($value, $bool) {
                return $bool ? $value : NULL;
            },
            true
        ),
        new ValidationStructure(
            "fromTime",
            function() {
                return true;
            },
            function($value, $shouldGet) {
                return $shouldGet && is_validTime($value);
            },
            function($value, $bool) {
                return $bool ? toMinutes($value) : NULL;
            },
            false
        ),
        new ValidationStructure(
            "toTime",
            function() {
                return true;
            },
            function($value, $shouldGet) {
                return $shouldGet && is_validTime($value);
            },
            function($value, $bool) {
                return $bool ? toMinutes($value) : NULL;
            },
            false
        ),
        new ValidationStructure(
            "gifts",
            function() {
                return true;
            },
            function($value, $shouldGet) {
                return is_arrayOfGifts($value);
            },
            function($value, $bool) {
                return $bool ? $value : NULL;
            },
            false
        ),
        new ValidationStructure(
            "giftCustom",
            function() use ($arguments) {
                return in_array("other", $arguments["gifts"]);
            },
            function($value, $shouldGet) {
                return !$shouldGet || is_nonEmptyString($value) && strlen($value) <= 100;
            },
            function($value, $bool) {
                return $bool ? $value : NULL;
            },
            false
        )
    ];

    $validFields = [];
    $invalidFieldNames = [];
    foreach ($formFields as $validationStructure)
    {
        $fieldValidation = false;
        $isset = array_key_exists($validationStructure->formFieldName, $arguments);
        $required = $validationStructure->isRequired;
        $initialValue = $arguments[$validationStructure->formFieldName];
        $optional = call_user_func($validationStructure->shouldGet, $validationStructure->formFieldName);
        $fieldValidation = call_user_func($validationStructure->validationFunction, $initialValue, $isset && $optional);
        $value =  call_user_func($validationStructure->valueGetter, $initialValue, $optional && $fieldValidation);

        validateField($validationStructure->formFieldName, $value, $fieldValidation, $required, $validFields, $invalidFieldNames);
    }

    return new ValidationResult(empty($invalidFieldNames), $validFields, $invalidFieldNames);
}

function validateField(string $name, $value, bool $result, bool $required, &$validFields, &$invalidFieldNames)
{
    if (!$result || ($required && empty($value)))
    {
        $invalidFieldNames[] = $name;
        return;
    }

    if (!empty($value) || $value === 0)
    {
        $validFields[$name] = $value;
    }
}


function is_nonEmptyString($value) : bool
{
    return is_string($value) && !empty($value);
}

function is_validTime($value) : bool
{
    if (empty($value))
    {
        return true;
    }

    if (!preg_match("/^([0-9]{1,2}):([0-9]{2})$/", $value, $matches))
    {
        return false;
    }

    $hours = (int)$matches[1];
    $minutes = (int)$matches[2];
    return (
        ($hours < 24 && $minutes < 60) ||
        ($hours == 24 && $minutes == 0)
    );
}

function toMinutes($value)
{
    if ($value != null && $value == 0 )
    {
        return 0;
    }

    if (empty($value))
    {
        return NULL;
    }

    $values = explode(":", $value);
    $result = 0;
    $result += (int)$values[0] * 60;
    $result += (int)$values[1];

    return $result;
}

function is_arrayOfGifts($value) : bool
{
    $giftOptions = ["socks", "points", "jarnik", "cash", "teddy", "other"];
    return (
        empty($value) || (
            is_array($value) &&
            count(array_intersect($value, $giftOptions)) == count($value)
        )
    );
}