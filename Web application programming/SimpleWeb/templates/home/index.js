document.addEventListener("DOMContentLoaded", buttonAdder)

function buttonAdder()
{
    let tableBody = document.getElementById("shopping-list-body");
    Array.from(tableBody.children).forEach(row =>
    {
        let editButton = createButton("edit-button", "EDIT", function()
        {
            makeEditable(row.id);
        });
        let deleteButton = createButton("delete-button", "DELETE", function()
        {
            // TODO
            alert("delete");
        });

        row.children[2].appendChild(editButton);
        row.children[2].appendChild(deleteButton);
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

function makeEditable(id)
{
    // TODO
    alert("edit");

    let tableBody = document.getElementById("shopping-list-body");
    changeDisplay(tableBody, "edit-button", "none");
    changeDisplay(tableBody, "delete-button", "none");

    let row = Array.from(tableBody.children).find(child => child.id == id);
    let saveButton = createButton("save-button", "SAVE", function()
    {
        // TODO Save plus redirect
        alert("save");

        changeToViewing(tableBody);
    });
    let cancelButton = createButton("cancel-button", "CANCEL", function() {
        changeToViewing(tableBody);
    });
    row.children[2].appendChild(saveButton);
    row.children[2].appendChild(cancelButton);

    let oldValue = row.children[1].textContent;
    row.children[1].textContent = null;
    let input = document.createElement("input");
    input.type = "number";
    input.min = 1;
    input.name = "edit-input";
    input.id = "edit-input";
    input.value = oldValue;
    row.children[1].appendChild(input);
}

function changeToViewing(tableBody)
{
    changeDisplay(tableBody, "edit-button", "block");
    changeDisplay(tableBody, "delete-button", "block");
    removeElementsByClass(tableBody, "edit-input")
    removeElementsByClass(tableBody, "save-button");
    removeElementsByClass(tableBody, "cancel-button");
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