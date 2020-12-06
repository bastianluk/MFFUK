class Position
{
    ValueX
    ValueY

    constructor (x, y)
    {
        this.ValueX = x;
        this.ValueY = y;
    }

    getX()
    {
        return this.ValueX;
    }

    getY()
    {
        return this.ValueY;
    }
}

class Tile
{
    Element
    Position

    constructor (element, position)
    {
        if (!(element instanceof HTMLElement))
        {
            throw new Error("Invalid argument");
        }
        this.Element = element;
        this.moveTo(position);
    }

    getPosition()
    {
        return this.Position;
    }

    moveTo(position)
    {
        this.Position = position;
        this.Element.style.left = this.Position.getX() * 20 + "vmin";
        this.Element.style.top = this.Position.getY() * 20 + "vmin";
    }
}


class LoydBoard
{
    Tiles
    EmptyTilePosition
    Width

    constructor (board)
    {
        if (!(board instanceof Element))
        {
            throw new Error("Invalid argument");
        }

        this.Width = 4;
        let height = 4;

        let index = 0;
        this.Tiles = [];
        for (let child of board.getElementsByClassName("game-tile"))
        {
            let Xcoordinate = index % this.Width;
            let Ycoordinate = Math.floor(index / this.Width);
            let position = new Position(Xcoordinate, Ycoordinate);
            let tile = new Tile(child, position);
            child.addEventListener("click", e =>
            {
                this.processMove(tile);
            });
            this.Tiles.push(tile);
            index++;
        }

        if (this.Tiles.length !== this.Width * height - 1)
        {
            throw new Error("Invalid board");
        }

        this.setDefault();

        this.randomize();
    }

    setDefault()
    {
        this.EmptyTilePosition = new Position(3,3);
    }

    randomize()
    {
        for (let n = 0; n < 1000; n++)
        {
            this.moveTile(this.Tiles[Math.floor(Math.random() * this.Tiles.length)]);
        }
    }

    moveTile(tile)
    {
        if (!this.Tiles.indexOf(tile) < 0)
        {
            throw new Error("Invalid argument");
        }

        let xOffset = this.EmptyTilePosition.getX() - tile.getPosition().getX();
        let yOffset = this.EmptyTilePosition.getY() - tile.getPosition().getY();
        let isMoveByOne = (Math.abs(xOffset) <= 1 && Math.abs(yOffset) <= 1);
        let isDiagonalMove = (Math.abs(xOffset) === 1 && Math.abs(yOffset) === 1);
        if (isMoveByOne && !isDiagonalMove)
        {
            let newEmptyPosition = tile.getPosition();
            tile.moveTo(this.EmptyTilePosition);
            this.EmptyTilePosition = newEmptyPosition
        }
    }

    isFinished()
    {
        for (let i = 0; i < this.Tiles.length; i++)
        {
            let isIncorrectXValue = this.Tiles[i].getPosition().getX() !== i % this.Width;
            let isIncorrectYValue = this.Tiles[i].getPosition().getY() !== Math.floor(i / this.Width)
            if (isIncorrectXValue || isIncorrectYValue)
            {
                return false;
            }
        }
        return true;
    }

    processMove(tile)
    {
        this.moveTile(tile);
        if (this.isFinished())
        {
            window.alert("Winner!");
        }
    }
}

document.addEventListener("DOMContentLoaded", gameOn)

function gameOn() {
    let gameState = new LoydBoard(document.getElementById("game-board"));
    // console.log(gameState);
}