<?php

/*
 * Create an array primes from 2 to $max.
 */
function sieve($max)
{
    if ($max <= 2) {
        return [];
    }

    $primes = range(2, $max);
    $position = 0;
    $index = 0;

    while($primes[$index] ** 2 < $max)
    {
        $number = $primes[$index];

        // Remove duplicates
        for ($i=1; $i < (($max / $number) + 1); $i++)
        {
            $innerIndex = $index + $number * $i;
            unset($primes[$innerIndex]);
        }

        do
        {
            $position++;
        }
        while (!array_key_exists($position, $primes));
        $index = $position;
    }

    return $primes;
}
?>