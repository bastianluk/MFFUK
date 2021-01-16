<?php

class SqlContext
{
    private $connection;

    public function __construct()
    {
        require __DIR__ . '/db_config.php';

        $this->connection = new mysqli($db_config['server'], $db_config['login'], $db_config['password'], $db_config['database']);
        if ($this->connection->connect_error)
        {
            die("Could not connect to the database");
        }
    }

    public function getAllListItems() : array
    {
        $query = 'SELECT i.id, i.name, l.amount, l.position FROM list AS l JOIN items AS i ON l.item_id = i.id';
        $query_result = self::execute($query);

        require_once(__DIR__ . "/../entities/listItem.php");

        $items = [];
        while ($row = $query_result->fetch_assoc())
        {
            $item = new ListItem($row['id'], $row['name'], $row['amount'], $row['position']);
            $items[] = $item;
        }

        return $items;
    }

    public function execute(string $query)
    {
        $query_result = $this->connection->query($query) or self::handle_error();

        return $query_result;
    }

// $query = 'update ' . $table . 'SET' . $values . 'WHERE id = "' . $this->connection->real_escape_string($id) . '"';

    private function handle_error()
    {
        die("Query error: " . $this->connection->error);
    }
}
