<?php
    // can edit!!
    // Example of input data (array of objects)
    $tasks = [
        (object)[
            'id' => 't1',                   // unique identifier
            'task' => 'Smash the anvil',    // string with text description of the task
            'assigned' => 'Thor',           // string with name (who is working on the assignment)
            'parent' => null,               // reference to an ID of another task (null = this is a top-level task)
        ],
        (object)[
            'id' => 't2',
            'task' => 'Keep the door closed',
            'assigned' => 'Heimdall',
            'parent' => null,
        ],
        (object)[
            'id' => 't2.1',
            'task' => 'Find the key and lock it',
            'assigned' => 'Freyr',
            'parent' => 't2', // means that this task is a sub-task of 't2' task
        ],
        (object)[
            'id' => 't3',
            'task' => 'Rule the Asgard',
            'assigned' => 'Odin',
            'parent' => null,
        ],
    ];

    // If data file is present in current directory, load $tasks data from that file.
    $inputFile = './tasks.json';
    if (is_readable($inputFile) && is_file($inputFile)) {
        $json = file_get_contents($inputFile);
        $tasks = json_decode($json, false);
    }

    /*
     * The objective is to modify, how the task data are visualized in HTML.
     * Replace the table with unordered list. Top level list items are top-level
     * tasks and each nested task is listed in sub-menu beneath its parent.
     * List item captions should contain the task description and the name of assigned person
     * (exact format is up to you).
     */



    /*
     * Your PHP code goes here (and you may modify HTML-interleaved code below).
     * Try to keep PHP and HTML separate as much as possible.
     */

    function isChild($parentId, $value) : bool
    {
      return $value->parent == $parentId;
    }

    // Will print children of the selected parent, will be called recurrsively
    function printChildren($values, $parentId)
    {
      // There has to be an out of the box way to do array.Where().
      $children = [];
      foreach ($values as $task) {
        if(isChild($parentId, $task))
        {
          $children[] = $task;
        }
      }

      if(!empty($children))
      {
        if($parentId != null)
        {
          echo "with sub-tasks:\n";
        }
        echo "<ul>\n";
        foreach ($children as $child)
        {
          echo "<li>\n";
          echo "Task -". htmlspecialchars($child->task) . "- for -" . htmlspecialchars($child->assigned) . "-\n";

          // Recurrsively look for subtasks - since I dont order the $tasks array it has to go over the entire array again to find all the subtasks, also doesn't prevent cycles.
          printChildren($values, $child->id);
          echo "</li>\n";
        }
        echo "</ul>\n";
      }
    }
?><!doctype html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <title>Project Planner</title>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css"
        integrity="sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2" crossorigin="anonymous">
</head>

<body class="container">
<h1 class="mt-4 mb-4">Project Planner</h1>

<?= printChildren($tasks, null) ?>

</body>

</html>