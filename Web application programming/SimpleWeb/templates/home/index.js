document.addEventListener("DOMContentLoaded", buttonAdder)

function buttonAdder()
{
    let tableBody = document.getElementById("shopping-list-body");

    // foreach row, add edit and delete buttons
    // on edit save and cancel buttons, the row becomes editable
    for (let row of tableBody.children)
    {
        let button = document.createElement("button");
        button.addEventListener("click", function()
        {
            alert("did something");
        });
        row.children[2].appendChild()
    }
}