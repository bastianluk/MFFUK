<?php

require_once(__DIR__ . '/solution.php');

function recodex_prepare_data(&$input)
{
  static $counter = 0;
  ++$counter;

  if (is_object($input) || is_array($input)) {
    foreach ($input as &$sub)
      recodex_prepare_data($sub);
  }

  if (is_object($input) && $counter % 2 == 0) {
    $input = (array)$input;  // flip every other object to associative array
  }
}

function recodex_run(&$argv) {
  array_shift($argv);  // skip script name
  $fileName = array_shift($argv);
  if (!$fileName)
    throw new Exception("No input file given.");

  $input = json_decode( file_get_contents($fileName) );
  if (!$input)
    throw new Exception("Given file '$fileName' does not hold a valid input.");

  recodex_prepare_data($input);

  try {
    $proc = new ConfigPreprocessor($input);
  }
  catch (Exception $e) {
    echo $e;
    echo "Exception thrown from constructor!\n";
    exit(0);
  }

  try {
    $result = json_encode($proc->getAllTasks());
    $result2 = json_encode($proc->getAllTasks());
    if ($result !== $result2) {
      echo "Repeated execution of getAllTasks() yields different results!\n";
    } else {
      echo $result;
    }
  }
  catch (Exception $e) {
    echo "Exception\n";  // some exceptions may be expected
    exit(0);
  }
}


try {
  recodex_run($argv);
}
catch (Exception $e)
{
  echo "Internal Error: ", $e->getMessage(), "\n";
  exit(1);
}
