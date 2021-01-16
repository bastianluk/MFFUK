<?php

class ListItem
{
    public $id;
    public $name;
    public $amount;
    public $position;


    public function __construct($id, $name, $amount, $position)
    {
        $this->id = $id;
        $this->name = $name;
        $this->amount = $amount;
        $this->$position = $position;
    }
}