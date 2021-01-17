document.addEventListener("DOMContentLoaded", eventAdder)

function eventAdder()
{
    let tableBody = document.getElementById("shopping-list-body");

    addClickEventsTo("down-button", function(button)
    {
        moveAction(tableBody, button, -1);
    });
    addClickEventsTo("up-button", function(button)
    {
        moveAction(tableBody, button, 1);
    });
    addClickEventsTo("edit-button", function(button)
    {
        editAction(tableBody, button);
    });
    addClickEventsTo("delete-button", function(button)
    {
        deleteAction(button);
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

function createButton(className, text, delegate)
{
    let button = document.createElement("button");
    button.classList.add(className);
    button.textContent = text;
    button.addEventListener("click", delegate);

    return button;
}

function moveAction(tableBody, button, change)
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
        changeToViewing(tableBody)
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

    let dataObject = {
        id: parseInt(row.id),
        position: parseInt(row.getAttribute("data-position"))
    };
    let formData = new FormData();
    let parameterKVPs = Object.entries(dataObject);
    for (let [key, value] of parameterKVPs)
    {
        formData.append(key, value);
    }

    fetchRequest(url, "POST", formData, "follow", function()
    {
        row.parentNode.removeChild(row);
    });
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
    console.log("here");
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


///
/// Fetch API
///
function fetchRequest(url, method, body, redirect, onSuccess)
{
    fetch(url,
    {
        method: method,
        body: body,
        redirect: redirect
    })
    .then(response =>
    {
        if (!response.ok && !response.redirected)
        {
            response.text()
            .then(text =>
            {
                console.log(text);
            })
            return;
        }

        if (response.redirected)
        {
            console.log("redirected");
        }

        onSuccess();
    })
    .catch((error) =>
    {
        console.log(error);
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