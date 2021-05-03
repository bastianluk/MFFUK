document.addEventListener("DOMContentLoaded", eventAdder)

function eventAdder()
{
    let tableBody = document.getElementById("shopping-list-body");

    addClickEventsTo("down-button", function(button)
    {
        moveAction(button, -1);
    });
    addClickEventsTo("up-button", function(button)
    {
        moveAction(button, 1);
    });
    addClickEventsTo("edit-button", function(button)
    {
        editAction(tableBody, button);
    });
    addClickEventsTo("delete-button", function(button)
    {
        deleteAction(button);
    });
    addClickEventsTo("mass-delete-button", function(button)
    {
        massDeleteAction();
    });
}

function addClickEventsTo(className, delegate)
{
    let buttons = document.getElementsByClassName(className);
    Array.from(buttons).forEach(button =>
    {
        button.addEventListener("click", function()
        {
            delegate(button);
        });
    });
}

function moveAction(button, change)
{
    let url = getPageUrl("move/index");
    let row = getRowFromButton(button);
    let oldPosition = parseInt(row.getAttribute("data-position"));
    let newPosition = oldPosition + change;
    let dataObject = {
        id: parseInt(row.id),
        oldPosition: oldPosition,
        newPosition: newPosition
    };
    let formData = objectToFormData(dataObject);

    fetchRequest(url, "POST", formData, "follow", function()
    {
        window.location.replace(getPageUrl("home/index"));
    });
}

function editAction(tableBody, button)
{
    changeDisplayOnButtons(tableBody, "none");
    let row = getRowFromButton(button);
    let saveButton = createButton("save-button", "SAVE", function()
    {
        saveAction(tableBody, row);
    });
    row.children[3].appendChild(saveButton);

    let cancelButton = createButton("cancel-button", "CANCEL", function()
    {
        changeToViewing(tableBody);
    });
    row.children[3].appendChild(cancelButton);

    let oldValue = row.children[1].textContent;
    row.children[1].textContent = null;
    let form = getEditFormElement(oldValue);
    row.children[1].appendChild(form);
}

function deleteAction(button)
{
    let row = getRowFromButton(button);
    let url = getPageUrl("delete/index");
    let rowPosition = parseInt(row.getAttribute("data-position"));

    let dataObject = {
        id: parseInt(row.id),
        position: rowPosition
    };
    let formData = objectToFormData(dataObject);

    fetchRequest(url, "POST", formData, "follow", function()
    {
        let tableBody = row.parentNode;
        Array.from(tableBody.children).forEach(childRow =>
        {
            let currentPosition = parseInt(childRow.getAttribute("data-position"));
            if (currentPosition > rowPosition)
            {
                let newPosition = currentPosition - 1;
                childRow.setAttribute("data-position", newPosition.toString());
            }
        });
        tableBody.removeChild(row);
    });
}

function massDeleteAction()
{
    let input = document.getElementById("mass-delete-input");
    let pattern = input.value.toString();
    let nameCells = document.getElementsByClassName("name-cell");
    let matchingNameCells = Array.from(nameCells).filter(cell => isMathing(cell, pattern));
    let names = matchingNameCells.map(cell => getNameCellValue(cell));
    let message = "Do you really want to delete:" + names.join(", ");

    if (names.length == 0)
    {
        window.alert("No matching elements found.");
        return;
    }

    if (window.confirm(message))
    {
        let url = getPageUrl("delete/ids");
        let ids = matchingNameCells.map(cell => getNameCellRowId(cell));
        let data = {
            ids: ids
        };
        fetchRequest(url, "POST", objectToFormData(data), "follow", function()
        {
            window.location.replace(getPageUrl("home/index"));
        });
    }
}

function saveAction(tableBody, row)
{
    let url = getPageUrl("edit/index")
    let forms = row.getElementsByClassName("edit-form");
    Array.from(forms).forEach(form => {
        let body = new FormData(form);
        let parentCell = form.parentElement;
        let parentRow = parentCell.parentElement;
        body.append("id", parentRow.id);
        fetchRequest(url, "POST", body, "follow", function()
        {
            changeToViewing(tableBody);
        });
    });
}

function changeToViewing(tableBody)
{
    changeDisplayOnButtons(tableBody, "block")
    changeAmountElement(tableBody);
    removeElementsByClass(tableBody, "save-button");
    removeElementsByClass(tableBody, "cancel-button");
}

function changeDisplayOnButtons(tableBody, display)
{
    changeDisplay(tableBody, "up-button", display);
    changeDisplay(tableBody, "down-button", display);
    changeDisplay(tableBody, "edit-button", display);
    changeDisplay(tableBody, "delete-button", display);
}

function getEditFormElement(initValue)
{
    let form = document.createElement("form");
    form.classList.add("edit-form");

    let input = document.createElement("input");
    input.type = "number";
    input.min = 1;
    input.classList.add("edit-input");
    input.name = "amount";
    input.id = "edit-input";
    input.value = initValue;

    form.appendChild(input);

    return form;
}

function isMathing(cell, pattern)
{
    let name = getNameCellValue(cell);
    let match = name.match(pattern);

    return match != null;
}

function getNameCellValue(cell)
{
    return cell.textContent;
}

function getNameCellRowId(cell)
{
    return cell.parentNode.id;
}

///
/// Fetch API
///
function fetchRequest(url, method, body, redirect, onSuccess)
{
    // Wouldnt need to return.
    return fetch(url,
    {
        method: method,
        body: body,
        redirect: redirect
    })
    .then(response =>
    {
        //console.log(response);
        if (!response.ok && !response.redirected)
        {
            response.text()
            .then(text =>
            {
                throw new Error(text);
            });
        }
        if (response.redirected)
        {
            window.location.replace(response.url);
        }
        else
        {
            onSuccess();
        }
    })
    .catch((error) =>
    {
        console.log(error);
        let messageBlock = createMessageBlock(error.message);
        document.body.insertBefore(messageBlock, document.body.firstChild);
    });
}


///
/// Helpers
///
function getRowFromButton(button)
{
    let cell = button.parentElement;
    return cell.parentElement;
}

function getPageUrl(page)
{
    let url = window.location.pathname;
    return url + "?page=" + page;
}

function createButton(className, text, delegate)
{
    let button = document.createElement("button");
    button.classList.add(className);
    button.textContent = text;
    button.addEventListener("click", delegate);

    return button;
}

function createMessageBlock(message)
{
    let block = document.createElement("div");
    block.classList.add("messageBlock");

    let title = document.createElement("h4");
    title.textContent = "Message";

    let paragraph = document.createElement("p");
    paragraph.textContent = message;

    block.appendChild(title);
    block.appendChild(paragraph);

    return block;
}

function objectToFormData(dataObject)
{
    let formData = new FormData();
    let parameterKVPs = Object.entries(dataObject);
    for (let [key, value] of parameterKVPs)
    {
        formData.append(key, value);
    }

    return formData;
}

function changeDisplay(parent, className, display)
{
    let elements = parent.getElementsByClassName(className)
    Array.from(elements).forEach(element => element.style.display = display);
}

function changeAmountElement(tableBody)
{
    let forms = tableBody.getElementsByClassName("edit-form")
    Array.from(forms).forEach(form =>
    {
        let parent = form.parentNode;
        let input = form.children[0]
        let value = input.value;
        parent.textContent = value;
    });
    removeElementsByClass(tableBody, "edit-form")
}

function removeElementsByClass(parent, className)
{
    var elements = parent.getElementsByClassName(className);
    while(elements.length > 0)
    {
        elements[0].parentNode.removeChild(elements[0]);
    }
}