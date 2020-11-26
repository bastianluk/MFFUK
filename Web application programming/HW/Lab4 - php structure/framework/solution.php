<?php

class Validator
{
  // Used to make validation of what is and what is not a task easier.

  public static function is_nonEmptyString($value): bool
  {
      return is_string($value) && !empty($value);
  }

  public static function is_arrayOfNonEmptyString($value): bool
  {
      return (
        is_array($value) &&
        array_reduce(
          $value,
          function ($carry, $item) {
              return $carry && self::is_nonEmptyString($item);
          },
          true
        )
      );
  }

  public static function is_task($data): bool
  {
    if (is_array($data))
    {
      if (
        isset($data['id']) &&
        isset($data['command']) &&
        isset($data['priority']) &&
        isset($data['dependencies'])
      )
      {
        return (
          self::is_nonEmptyString($data['id']) &&
          self::is_nonEmptyString($data['command']) &&
          is_int($data['priority']) &&
          self::is_arrayOfNonEmptyString($data['dependencies'])
        );
      }
    }

    if (is_object($data))
    {
      if (
        property_exists($data, 'id') &&
        property_exists($data, 'command') &&
        property_exists($data, 'priority') &&
        property_exists($data, 'dependencies')
      )
      {
        return (
          Validator::is_nonEmptyString($data->id) &&
          Validator::is_nonEmptyString($data->command) &&
          is_int($data->priority) &&
          Validator::is_arrayOfNonEmptyString($data->dependencies)
        );
      }
    }

    return false;
  }
}

class Task
{
  // Number given by the parser - represents the order of when it was parsed.
  public $parseId;

  // From the structure
  public $id;

  //public $command;

  // From the structure
  public $priority;

  // From the structure - dependencies
  public $parentIds;

  // Calculated from the structure - after all tasks are loaded, based on parentIds children are also referenced.
  public $childIds;

  // The original structure
  public $structure;

  private function __construct($data, $parseId)
  {
      $this->structure = $data;
      $this->parseId = $parseId;
      if (is_array($data)) {
          $this->id = $data['id'];
          $this->priority = $data['priority'];
          $this->parentIds = $data['dependencies'];
      } else {
          $this->id = $data->id;
          $this->priority = $data->priority;
          $this->parentIds = $data->dependencies;
      }
      $this->childIds = [];
  }

  public static function create($data, $parseId) : Task
  {
    if (!Validator::is_task($data))
    {
        throw new InvalidArgumentException("Invalid data");
    }

    return  new Task($data, $parseId);
  }

  // Used in debugging.
  public function print()
  {
      echo "Task:\n  \tParsedId:'$this->parseId',\n \tId:'$this->id',\n \tPrio:'$this->priority',\n \tDep:\n";
      foreach ($this->parentIds as $parentId)
      {
        echo "\t\t'$parentId',\n";
      }
      echo "\tChild:\n";
      foreach ($this->childIds as $childId)
      {
        echo "\t\t'$childId',\n";
      }
      echo "\n";
  }
}

class ConfigPreprocessor
{
  private $tasks;

  private $parentTasks;

  public function __construct($config)
  {
    $this->tasks = self::parse($config);
    $this->parentTasks = self::create($this->tasks);
  }

  private function parse($config)
  {
    $result = [];
    $counter = 0;

    $queue = [$config];
    while(!empty($queue))
    {
      $value = array_shift($queue);

      // With this, the check in the Task.Create is not needed, just used to having create methods that can fail/validate on call.
      if (Validator::is_task($value))
      {
        $createdTask = Task::create($value, $counter);
        $counter++;
        $result[$createdTask->id] = $createdTask;
      }

      // Search the nested elemetns - thanks test 9. :>
      if (is_array($value) || is_object($value))
      {
        foreach ($value as $childValue)
        {
          $queue[] = $childValue;
        }
      }
    }

    // After everything is parsed, reference the child tasks as well.
    foreach ($result as $parsedTask)
    {
      foreach ($parsedTask->parentIds as $parentId)
      {
        // Not sure this is needed, but user input is always worth validating.
        if (!isset($result[$parentId]))
        {
          throw new InvalidArgumentException("Parent task does not exist.");
        }

        $result[$parentId]->childIds[] = $parsedTask->id;
      }
    }

    return $result;
  }

  private function create($tasks)
  {
    $result = [];

    foreach ($tasks as $task)
    {
      if (empty($task->parentIds))
      {
        $result[] = $task;
      }
    }

    return $result;
  }

  // Was used in debugging.
  private static function printTaskArray(string $title, $tasks)
  {
    echo $title, "\n";
    foreach ($tasks as $task)
    {
      $task->print();
    }
    echo "---\n";
  }

  /**
   * Get an array of tasks from the config in the right order.
   */
  public function getAllTasks() : array
  {
    if (empty($this->parentTasks))
    {
      throw new UnexpectedValueException("There were no tasks that didn't have a root.");
    }

    // Wrapper so I dont have a global state per se.
    return self::orderTasks($this->tasks, $this->parentTasks);
  }

  private static function orderTasks(array $tasks, array $parentTasks) : array
  {
    $result = [];
    $queues = [];

    // Cannot be a state on the property, two runs wouldn't have the same results.
    $taskUnprintedParentCount = [];
    foreach ($tasks as $id => $task)
    {
      $taskUnprintedParentCount[$id] = count($task->parentIds);
    }

    $addToQueue = $parentTasks;
    while (true)
    {
      $queues = self::insertToQueues($addToQueue, $queues);

      // Get the best queue.
      $allQueuesEmpty = true;
      foreach ($queues as &$queue)
      {
        $allQueuesEmpty = empty($queue);
        if (!$allQueuesEmpty)
        {
            break;
        }
      }

      // If all queues were processed, break.
      if ($allQueuesEmpty)
      {
        break;
      }

      $topTask = array_shift($queue);
      $result[] = $topTask;

      $addToQueue = [];
      foreach ($topTask->childIds as $childId)
      {
        $taskUnprintedParentCount[$childId]--;
        if ($taskUnprintedParentCount[$childId] == 0)
        {
          $addToQueue[] = $tasks[$childId];
        }
      }
    }

    return self::getStructures($result);
  }

  private static function insertToQueues(array $tasksToAdd, array $queues) : array
  {
    foreach ($tasksToAdd as $queueReadyTask)
    {
      // If unseen priority, add new queue.
      if (!isset($queues[$queueReadyTask->priority]))
      {
          $queues[$queueReadyTask->priority] = [];
          krsort($queues);
      }

      // In to the queue insert to the correct position based on the order of parsing.
      $insertPosition = count($queues[$queueReadyTask->priority]);
      while (
        $insertPosition > 0 &&
        $queueReadyTask->parseId < $queues[$queueReadyTask->priority][$insertPosition - 1]->parseId
      )
      {
          $insertPosition--;
      }

      // The insert it self.
      array_splice($queues[$queueReadyTask->priority], $insertPosition, 0, [$queueReadyTask]);
    }

    return $queues;
  }

  private static function getStructures(array $tasks)
  {
    return array_map(
      function (Task $task)
      {
          return $task->structure;
      },
      $tasks
    );
  }
}
