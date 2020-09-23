# Devil mines

## How to start

Use the following to be presented with the game board:
`playDevilMines`

Then use the row and column in the following format as input for what field to uncover:
`7a`

In case of wrong input the turn number will not move. 

Note: To reset after a wrong input just press enter a couple of times and once a fresh field is presented you should be good to go. 

## Idea behind the game / Rules

Everyone knows the good old Minesweeper - this builds on the idea of it.

Same as in the basic game - you play on an 8 x 8 field where 10 mines are hidden. Your job is to uncover the field and leave just the places where the mines are untouched.

That would be easy enough on it's own but once less than 30% of the field is left uncovered, the Devil comes in to play.

The Devil is a mechanic that forces you NOT TO guess. If you randomly pick a place in the field and there is a mine, you lose just as you would in the normal game. If there is a possibility (more on that in the technical details section) that a mine could be under the place you chose, the Devil "put it" there and you lose the game. If it is impossible for the bomb to be there, you can keep playing. 

Note: There is no way to mark fields as mines with a flag in this implementation.

## Technical details

### The Devil

It uses a brute force approach which takes the unrevealed places on the board, the N number of mines that should be on the field and it tries all the combinations how to choose the N from those unrevealed fields. Then it counts the neighbouring mines for all the places in the field and compares them place by place to your original playing field. If the values on all the revelead places from your original field match the values on at least one generated playing field there is a possibility that a mine is in the place that you chose and you will lose. 

Could be improved by implementing a backtracking algorithm to determine the results faster than the method used.

### Mentions and sources

A part that was copied from a source is the use of a shuffling function to create the random positions for the mines it comes from the [haskell wiki](https://wiki.haskell.org/Random_shuffle) - in my case it is the: `shuffle :: [a] -> StdGen -> [a]`. 