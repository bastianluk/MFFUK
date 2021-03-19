<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");

//validateCall();

require_once(__DIR__ . "/_data.php");
$listItems = get_listItems();
$listCount = count($listItems);
$columns = [
    'listItem' => 'Item',
    'amount' => 'Amount',
    'position' => '',
    'edit' => ''
];
?>

<h2>Shopping list</h2>

<datalist id="knowItems">
    <?php foreach ($listItems as $listItem) { ?>
        <option value="<?= htmlspecialchars($listItem->item->name) ?>">
    <?php } ?>
</datalist>

<table class="table table-striped mt-5">
    <thead>
        <tr>
            <?php foreach ($columns as $caption) { ?>
            <th class="text-nowrap">
                <span class="mr-2 text-nowrap"><?= $caption ?></span>
            </th>
            <?php } ?>
        </tr>
    </thead>
    <tbody id="shopping-list-body">
        <?php foreach ($listItems as $key => $listItem) { ?>
        <tr id="<?= $listItem->id ?>" data-position="<?= $listItem->position ?>">
            <td class="w-75 name-cell"><?= htmlspecialchars($listItem->item->name) ?></td>
            <td class="w-25"><?= $listItem->amount ?></td>
            <td class="w-25">
                <button class="down-button">/\</button>
                <button class="up-button">\/</button>
            </td>
            <td class="w-25">
                <button class="edit-button">EDIT</button>
                <button class="delete-button">DELETE</button>
            </td>
        </tr>
        <?php } ?>
    </tbody>
</table>

<h2>Add to the list</h2>
<form action="?page=add/index" method="post" id="newItem">
    <div class="input">
        <label for="name">Name:</label>
        <input list="knowItems" name="name" id="name">
        <label for="amount">Amount:</label>
        <input type="number" name="amount" id="amount" min="1">
        <input type="text" name="redirectOnPost" id="redirect" value="true" style = "display: none">
    </div>

    <div class="input">
        <input type="submit" value="Add"/>
    </div>
</form>

<h2>Mass delete</h2>
<div class="input">
    <form id="mass-delete-form">
        <div class="input">
            <label for="mass-delete-input">Delete 'by' patern</label>
            <input type="text" name="mass-delete-input" id="mass-delete-input">
        </div>
    </form>

    <div class="input">
        <button class="mass-delete-button">DELETE ALL</button>
    </div>
</div>

<script src="templates/home/index.js"></script>