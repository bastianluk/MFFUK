<?php

require_once(__DIR__ . "/../../lib/subpage_lib.php");

//validateCall();

require_once(__DIR__ . "/_data.php");
$data = get_data();
$columns = [
    'item' => 'Item',
    'amount' => 'Amount',
    'edit' => '',
];
?>

<h2>Shopping list</h2>

<datalist id="knowItems">
  <option value="Edge">
  <option value="Firefox">
  <option value="Chrome">
  <option value="Opera">
  <option value="Safari">
</datalist>

<table class="table table-striped mt-5">
    <thead>
        <tr>
            <?php foreach ($columns as $col => $caption) { ?>
            <th class="text-nowrap">
                <span class="mr-2 text-nowrap"><?= $caption ?></span>
            </th>
            <?php } ?>
        </tr>
    </thead>
    <tbody>
        <?php foreach ($data as $item) { ?>
        <tr>
            <td class="w-50"><?= htmlspecialchars("$item->Name"); ?></td>
            <td class="w-25"><?= $item->Amount ?></td>
            <td class="w-25"></td>
        </tr>
        <?php } ?>
    </tbody>
</table>

<h2>Add new item</h2>
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