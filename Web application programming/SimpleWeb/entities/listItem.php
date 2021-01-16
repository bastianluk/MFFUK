<?php

class ListItem
{
    public $id;
    public $amount;
    public $position;


    public function __construct($id, $amount, $position)
    {
        $this->id = $id;
        $this->amount = $amount;
        $this->$position = $position;
    }
}