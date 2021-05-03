<?php

define('DATA_FILE', __DIR__ . '/data.json');

/**
 * Create and send JSON response.
 */
function json_response($payload = null, $error = '')
{
	header('Content-Type: application/json');
	$res = [ 'ok' => !$error ];
	if ($error) $res['error'] = $error;
	if ($payload) $res['payload'] = $payload;
	echo json_encode($res);
	exit;
}

/**
 * Save internal data back to data file.
 */
function save_data($data)
{
	file_put_contents(DATA_FILE, json_encode($data, JSON_PRETTY_PRINT));
}

/**
 * Safe way how to fetch value from an array and validate it via regex.
 */
function safe_get(array $params, string $name, $default = null, $regexCheck = null)
{
	if (!array_key_exists($name, $params)) return $default;
	if ($regexCheck && !preg_match($regexCheck, $params[$name])) return $default;
	return $params[$name];
}


/*
 * REST API Methods
 */

/**
 * Default GET action. Retrieve all records (without hours).
 */
function rest_get_default($data)
{
	foreach ($data as $rec)
		unset($rec->hours);
	json_response($data);
}


/**
 * Return single record (selected by its ID) with hours.
 */
function rest_get_hours($data)
{
	$id = (int)safe_get($_GET, 'id', null, '/^[0-9]+$/');
	if ($id > 0) {
		foreach ($data as $rec) {
			if ($rec->id == $id) {
				json_response($rec);
			}
		}
		json_response(null, "Record with ID #$id not found.");
	}
	else
		json_response(null, 'Invalid ID!');
}


/**
 * Update the hours for given record.
 */
function rest_post_hours($data)
{
	$id = (int)safe_get($_GET, 'id', null, '/^[0-9]+$/');
	$hours = safe_get($_GET, 'hours', null, '/^[0-9]+$/');
	if ($id > 0) {
		if ($hours !== null) {
			foreach ($data as $rec) {
				if ($rec->id == $id) {
					$rec->hours = (int)$hours;
					save_data($data);
					json_response(); // just send OK
				}
			}
			json_response(null, "Record with ID #$id not found.");
		}
		else
			json_response(null, "Invalid hours value provided.");
	}
	else
		json_response(null, 'Invalid ID!');

}


/**
 * Main function.
 */
function run()
{
	if (file_exists(DATA_FILE)) {
		$data = json_decode(file_get_contents(DATA_FILE));
	}
	else {
		$data = [];
	}

	$action = safe_get($_GET, 'action', 'default', '/^[a-z_]+$/');
	$method = strtolower($_SERVER['REQUEST_METHOD']);
	$target = "rest_${method}_${action}";

	if (function_exists($target))
		$target($data);
	else
		json_response(null, 'Target action or method is not available.');
}


try {
	run();
}
catch (Exception $e) {
	http_response_code(500);
	header('Content-Type: text/plain');
	echo $e->getMessage();
}
