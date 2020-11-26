<?php

class Validator
{
  public static function isNonEmptyString($value): bool
  {
      return is_string($value) && !empty($value);
  }

  public static function isNonEmptyStringArray($value): bool
  {
      return (
        is_array($value) &&
        array_reduce(
          $value,
          function ($carry, $item) {
              return $carry && self::isNonEmptyString($item);
          },
          true
        )
      );
  }

  public static function isTask($data): bool
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
          Validator::isNonEmptyString($data['id']) &&
          Validator::isNonEmptyString($data['command']) &&
          is_int($data['priority']) &&
          Validator::isNonEmptyStringArray($data['dependencies'])
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
          Validator::isNonEmptyString($data->id) &&
          Validator::isNonEmptyString($data->command) &&
          is_int($data->priority) &&
          Validator::isNonEmptyStringArray($data->dependencies)
        );
      }
    }

    return false;
  }
}

class Task
{
  public $id;

  public $command;

  public $priority;

  public $dependencies;

  public $structure;

  private function __construct($data)
  {
      $this->structure = $data;
      if (is_array($data)) {
          $this->id = $data['id'];
          $this->priority = $data['priority'];
          $this->dependencies = $data['dependencies'];
      } else {
          $this->id = $data->id;
          $this->priority = $data->priority;
          $this->dependencies = $data->dependencies;
      }
  }

  public static function create($data) : Task
  {
    if (!Task::isTask($data))
    {
        throw new InvalidArgumentException("Invalid data");
    }

    return  new Task($data);
  }
}

class ConfigPreprocessor
{
  private $tasks;

  private $taskDependecies;

  public function __construct($config)
  {
    $this->tasks = parse($config);
    $this->taskDependecies = create($tasks);
  }

  private function parse($config) : array
  {
    $result = [];

    $queue = [$config];
    while(!empty($queue))
    {
      $value = array_shift($queue);
      if (Validator::isTask($value))
      {
        $task = Task::create($value);
        $tasks[$task->id] = $task;
        continue;
      }

      if (is_array($value) || is_object($value))
      {
        foreach ($value as $childValue) {
          array_merge($queue, $childValue);
        }
        continue;
      }
    }

    return $result;
  }

  private function create($tasks) : array
  {
    $result = [];
    return $result;
  }

  /**
   * Get an array of tasks from the config in the right order.
   */
  public function getAllTasks() : array
  {
    if (empty($this->taskDependecies))
    {
      throw new InvalidArgumentException("Invalid dependency tree");
    }

    $result = [];

    return [];
  }
}
