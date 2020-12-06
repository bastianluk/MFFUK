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
    $formFields = ["firstName", "lastName", "email", "deliveryBoy", "unboxDay", "fromTime", "toTime", "gifts", "giftCustom"];
    $deliveryBoys = ["jesus", "santa", "moroz", "hogfather", "czpost", "fedex"];
    $days = ["24", "25"];

    $validFields = [];
    $invalidFieldNames = [];
    foreach ($formFields as $field)
    {
        $fieldValidation = false;
        $required = false;
        $isset = array_key_exists($field, $arguments);
        $value = $arguments[$field];
        switch ($field) {
            case "firstName":
            case "lastName":
                $fieldValidation = is_nonEmptyString($value) && strlen($value) <= 100;
                $required = true;
                break;
            case "email":
                $fieldValidation = is_nonEmptyString($value) && filter_var($value, FILTER_VALIDATE_EMAIL) && strlen($value) <= 200;
                $required = true;
                break;
            case "deliveryBoy":
                $fieldValidation = in_array($value, $deliveryBoys);
                $required = true;
                break;
            case "unboxDay":
                $fieldValidation = in_array($value, $days);;
                $required = true;
                break;
            case "fromTime":
            case "toTime":
                $fieldValidation = $isset && is_validTime($value);
                $value =  $fieldValidation ? toMinutes($value) : NULL;
                break;
            case "gifts":
                $fieldValidation = is_arrayOfGifts($value);
                break;
            case "giftCustom":
                $shouldGet = in_array("other", $arguments["gifts"]);
                $fieldValidation = !$shouldGet || is_nonEmptyString($value) && strlen($value) <= 100;
                $value = $shouldGet ? $value : NULL;
                break;
            default:
                echo "problem";
        }

        validateField($field, $value, $fieldValidation, $required, $validFields, $invalidFieldNames);
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