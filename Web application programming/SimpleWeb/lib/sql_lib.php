<?php

require_once(__DIR__ . "/../entities/item.php");
require_once(__DIR__ . "/../entities/listItem.php");

class SqlContext
{
    private $connection;

    public function __construct()
    {
        require __DIR__ . '/db_config.php';

        $this->connection = new mysqli($db_config['server'], $db_config['login'], $db_config['password'], $db_config['database']);
        if ($this->connection->connect_error)
        {
            throw new Exception("Connect error:" . $this->connection->connect_error);
        }
    }

    public function getAllListItems() : array
    {
        $query = 'SELECT l.id, l.item_id, i.name, l.amount, l.position FROM list AS l JOIN items AS i ON l.item_id = i.id';
        $queryResult = self::execute($query);

        $items = [];
        while ($row = $queryResult->fetch_assoc())
        {
            $item = new Item($row['item_id'], $row['name']);
            $listItem = new ListItem($row['id'], $item, $row['amount'], $row['position']);
            $items[] = $listItem;
        }

        return $items;
    }

    public function upsertItemToList(string $name, int $amount)
    {
        $item = self::findItemByName($name);
        if(!isset($item))
        {
            $item = self::createItem($name);
        }

        self::updateAmountInList($item, $amount);
    }


    public function setAmount(int $id, int $amount)
    {
        $updateQuery = "UPDATE list SET amount = $amount WHERE id = $id";
        $updateQueryResult = self::execute($updateQuery);
    }

    public function deleteListItemAt($id, $position)
    {
        $deleteQuery = "DELETE FROM list WHERE id = $id";
        $deleteQueryResult = self::execute($deleteQuery);

        $updateQuery = "UPDATE list SET position = position - 1 WHERE position > $position";
        $updateQueryResult = self::execute($updateQuery);
    }

    public function moveListItemTo(int $id, int $oldPosition, int $newPosition)
    {
        $updateSpecificQuery = "UPDATE list SET position = $newPosition WHERE id = $id";
        $updateSpecificQueryResult = self::execute($updateSpecificQuery);

        if ($oldPosition > $newPosition)
        {
            $updateQuery = "UPDATE list SET position = position + 1 WHERE id <> $id AND $newPosition <= position AND position < $oldPosition";
            $updateQueryResult = self::execute($updateQuery);
        }

        if ($oldPosition < $newPosition)
        {
            $updateQuery = "UPDATE list SET position = position - 1 WHERE id <> $id AND $oldPosition < position AND position <= $newPosition";
            $updateQueryResult = self::execute($updateQuery);
        }
    }

    private function findItemByName(string $name, $default = null)
    {
        $itemQuery = 'SELECT * FROM items WHERE name = "' . $this->connection->real_escape_string($name) . '"';
        $itemQueryResult = self::execute($itemQuery);

        if ($row = $itemQueryResult->fetch_assoc())
        {
            $item = new Item($row['id'], $row['name']);
            return $item;
        }

        return $default;
    }

    private function createItem(string $name) : Item
    {
        $createQuery = 'INSERT INTO items (name) VALUES ("' . $this->connection->real_escape_string($name) . '")';
        $createQueryResult = self::execute($createQuery);

        return self::findItemByName($name);
    }

    private function updateAmountInList($item, int $amount)
    {
        $listItemId = self::findIdOfListItemIByItemId($item->id);
        if(!isset($listItemId))
        {
            $listItemId = self::createListItemIByItemId($item->id);
        }

        $oldAmount = self::getListItemAmountById($listItemId);
        $newAmount = $oldAmount + $amount;
        self::setAmount($listItemId, $newAmount);
    }

    private function findIdOfListItemIByItemId(int $item_id, $default = null)
    {
        $itemQuery = "SELECT id FROM list WHERE item_id = $item_id";
        $itemQueryResult = self::execute($itemQuery);

        if ($row = $itemQueryResult->fetch_assoc())
        {
            return $row['id'];
        }

        return $default;
    }

    private function createListItemIByItemId(int $item_id)
    {
        $maxPosition = self::getMaxListPosition();
        $newPosition = $maxPosition + 1;
        $createQuery = "INSERT INTO list (item_id, amount, position) VALUES ($item_id, 0, $newPosition)";
        $createQueryResult = self::execute($createQuery);

        return self::findIdOfListItemIByItemId($item_id);
    }

    public function getMaxListPosition()
    {
        $maxQuery = 'SELECT MAX(position) AS maxPos FROM list';
        $maxQueryResult = self::execute($maxQuery);
        if ($row = $maxQueryResult->fetch_assoc())
        {
            return $row['maxPos'];
        }

        return 0;
    }

    private function getListItemAmountById($id)
    {
        $itemQuery = "SELECT amount FROM list WHERE id = $id";
        $itemQueryResult = self::execute($itemQuery);

        if ($row = $itemQueryResult->fetch_assoc())
        {
            return $row['amount'];
        }

        return 0;
    }

    private function execute(string $query)
    {
        $queryResult = $this->connection->query($query) or self::handle_error();

        return $queryResult;
    }

    private function handle_error()
    {
        throw new Exception("Query error: " . $this->connection->error);
    }
}