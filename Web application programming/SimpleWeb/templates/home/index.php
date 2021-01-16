<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");

//validateCall();

require_once(__DIR__ . "/_data.php");
$listItems = get_listItems();
$columns = [
    'listItem' => 'Item',
    'amount' => 'Amount',
    'edit' => '',
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
        <?php foreach ($listItems as $listItem) { ?>
        <tr>
            <td class="w-50"><?= htmlspecialchars($listItem->item->name); ?></td>
            <td class="w-25"><?= $listItem->amount ?></td>
            <td class="w-25"></td>
        </tr>
        <?php } ?>
    </tbody>
</table>

<h2>Add new listItem</h2>
<form action="?page=add/index" method="post" id="newItem">
    <div class="input">
        <label for="name">Name:</label>
        <input list="knowItems" name="name" id="name">
    </div>
    <div class="input">
        <label for="amount">Amount:</label>
        <input type="number" name="amount" id="amount" min="0">
    </div>

    <div class="input">
        <input type="submit" value="Add"/>
    </div>
</form>

<script src="index.js" type="text/javascript"></script>