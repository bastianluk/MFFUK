<?php

class ListItem
{
    public $id;
    public $item;
    public $amount;
    public $position;


    public function __construct($id, $item, $amount, $position)
    {
        $this->item = $item;
        $this->amount = $amount;
        $this->$position = $position;
    }
}