document.addEventListener("DOMContentLoaded", buttonAdder)

function buttonAdder()
{
    let tableBody = document.getElementById("shopping-list-body");

    for (let row of tableBody.children)
    {
        let button = document.createElement("button");
        row.children[2].appendChild()
    }
}