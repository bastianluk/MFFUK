document.addEventListener("DOMContentLoaded", buttonAdder)

function buttonAdder()
{
    let tableBody = document.getElementById("shopping-list-body");
    let rows = Array.from(tableBody.children);
    rows.forEach((row, index) =>
    {
        if (index > 0)
        {
            let moveDownButton = createButton("down-button", "/\\", function()
            {
                move(row, -1);
            });
            row.children[2].appendChild(moveDownButton);
        }

        if (index < array.length)
        {
            let moveUpButton = createButton("up-button", "\\/", function()
            {
                move(row, +1);
            });
            row.children[2].appendChild(moveUpButton);
        }
        let editButton = createButton("edit-button", "EDIT", function()
        {
            makeEditable(row.id);
        });
        let deleteButton = createButton("delete-button", "DELETE", function()
        {
            // USE AJAX
            // TODO Call correct thing
            alert("delete");
            let url = window.location.pathname;
            let parametrizedUrl = url + "?page=delete/index&id=" + row.id + "&position=" + row.position;
            let method = "POST";
            let result = fetch(parametrizedUrl, { method });
        });

        row.children[3].appendChild(editButton);
        row.children[3].appendChild(deleteButton);
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

function move(row, change)
{
    let oldPosition = row.position
    let newPosition = oldPosition + change;
    let parametrizedUrl = url + "?page=move/index&id=" + row.id + "&oldPosition=" + oldPosition + "&newPosition=" + newPosition;
    let method = "POST";
    let result = fetch(parametrizedUrl, { method });
}

function makeEditable(id)
{
    let tableBody = document.getElementById("shopping-list-body");
    changeDisplay(tableBody, "edit-button", "none");
    changeDisplay(tableBody, "delete-button", "none");

    let row = Array.from(tableBody.children).find(child => child.id == id);
    let saveButton = createButton("save-button", "SAVE", function()
    {
        // TODO Save plus redirect
        // Use new FormData() ?
        alert("save");
        let url = window.location.pathname;
        console.log(url);
        let amount = row.children[1].find('input').val();
        let parametrizedUrl = url + "?page=edit/index&id=" + id + "&amount=" + amount;
        let method = "POST";
        let result = fetch(parametrizedUrl, { method });

        changeToViewing(tableBody);
    });
    let cancelButton = createButton("cancel-button", "CANCEL", function()
    {
        changeToViewing(tableBody);
    });
    row.children[3].appendChild(saveButton);
    row.children[3].appendChild(cancelButton);

    let oldValue = row.children[1].textContent;
    row.children[1].textContent = null;

    let form = document.createElement("form");
    form.classList.add("edit-form");

    let input = document.createElement("input");
    input.type = "number";
    input.min = 1;
    input.name = "edit-input";
    input.id = "edit-input";
    input.value = oldValue;

    form.appendChild(input);
    row.children[1].appendChild(form);
}

function changeToViewing(tableBody)
{
    changeDisplay(tableBody, "edit-button", "block");
    changeDisplay(tableBody, "delete-button", "block");
    changeAmountElement(tableBody);
    removeElementsByClass(tableBody, "save-button");
    removeElementsByClass(tableBody, "cancel-button");
}

function changeAmountElement(tableBody)
{
    let forms = tableBody.getElementsByClassName("edit-form")
    Array.from(forms).forEach(form =>
    {
        let parent = form.parentNode;
        let value = form.children[0].value;
        parent.textContent = value;
    });
    removeElementsByClass(tableBody, "edit-form")
}

function changeDisplay(parent, className, display)
{
    let elements = parent.getElementsByClassName(className)
    Array.from(elements).forEach(element => {
        element.style.display = display;
    });;
}

function removeElementsByClass(parent, className)
{
    var elements = parent.getElementsByClassName(className);
    while(elements.length > 0)
    {
        elements[0].parentNode.removeChild(elements[0]);
    }
}