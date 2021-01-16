<?php

class ListItem
{
    public $item;
    public $amount;
    public $position;


    public function __construct($item, $amount, $position)
    {
        $this->item = $item;
        $this->amount = $amount;
        $this->$position = $position;
    }
}