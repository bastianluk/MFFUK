<?php

require_once __DIR__ . '/router.php';


try {
	// Get the router and dispatch the request
	$router = new Router();
	$router->dispatch();
}
catch (Exception $e) {
	// Error logging is very important. This should be done much better...
	die("Uncaught Exception: " . $e->getMessage());
}
