<?php

class SqlContext
{
    private $connection;

    public function __construct()
    {
        $db_config = require __DIR__ . '/db_config.php';

        $this->connection = new mysqli($db_config['server'], $db_config['login'], $db_config['password'], $db_config['database']);
        if ($this->connection->connect_error)
        {
            die("Could not connect to the database");
        }

        return $this->connection;
    }

    public function select($table, $id)
    {
        $query = 'select * FROM ' . $table . ' WHERE id = "' . $this->connection->real_escape_string($id) . '"';
        $query_result = $this->connection->query($query) or $this->handle_error($this->connection);

        return $query_result;
    }

    public function update($table, $id, $values)
    {
        $query = 'update ' . $table . 'SET' . $values . 'WHERE id = "' . $this->connection->real_escape_string($id) . '"';
        $query_result = $this->connection->query($query) or $this->handle_error($this->connection);

        return $query_result;
    }

    private function handle_error($connection)
    {
        die("Query error: " . $this->connection->error);
    }
}
