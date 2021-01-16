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
    let cancelButton = createButton("cancel-button", "CANCEL", function()
    {
        changeToViewing(tableBody);
    });
    row.children[2].appendChild(saveButton);
    row.children[2].appendChild(cancelButton);

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