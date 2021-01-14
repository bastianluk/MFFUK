class Rectangle
{
    left;
    top;
    width;
    height;
    finished;

    constructor(left, top, width, height, finished)
    {
        this.left = left;
        this.top = top;
        this.width = width;
        this.height = height;
        this.finished = finished;
    }

    getSize()
    {
        return width * height;
    }

    getEndWidth()
    {
        return this.left + this.width;
    }

    getEndHeight()
    {
        return this.top + this.height;
    }

    collidesWith(rect)
    {
        return (
            (this.left < rect.getEndWidth() && rect.left < this.getEndWidth()) ||
            (this.end < rect.getEndHeight() && rect.end < this.getEndHeight())
        );
    }
}

function maxFreeRect(width, height, rects)
{
    let max = new Rectangle(0, 0, 0, 0, 1);
    let blocks = [];
    let active = [];
    for (let sweeper = 0; sweeper < width; sweeper++)
    {
        let newBlocks = rects.filter(r => r.left == sweeper).map(r => new Rectangle(r.left, r.top, r.width, r.height, 0));
        blocks.concat(newBlocks);
        recalcActive(active, newBlocks, sweeper);
        blocked();
    }

    return {
        left: max.left, top: max.top, width: max.width, height: max.height
    };
}

function recalcActive(active, blocks, sweeper)
{
    let newActive = []
    for (const activeRect of active)
    {
        blocks.filter(b => b.collidesWith());
    }
}


// In nodejs, this is the way how export is performed.
// In browser, module has to be a global varibale object.
module.exports = { maxFreeRect };
