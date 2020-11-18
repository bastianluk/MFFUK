<?php

require_once(__DIR__ . '/solution.php');

$limit = (int)$argv[1];
$primes = sieve($limit);
foreach ($primes as $prime)
    echo $prime, "\n";
?>