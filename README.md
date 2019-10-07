# Tic-Tac-Toe, in Haskell
I wanted to learn Haskell, so I implemented a stupid Tic-Tac-Toe in it.
It is two player only, and X goes first. As fun as it was to write, I'll
probably never add AI for a single player mode. :(


# Requirements
Make & GHC

# To build & run
1. Clone this repository 
2. Navigate to the directory to which the repo was cloned
3. Run `make`
4. Run `./ttt`

# How to play
X goes first. Player 1 (X) presses a key 1-9 to choose a cell, then <CR> to
submit his/her move. Player 2 (O) does the same. If an invalid move is
submitted, the player who submitted the invalid move simply gets to choose
a new cell. Invalid moves include numbers outside of the range [1, 9], and
cells which already have an X or an O. The first player to get a line of three
of his/her pieces (row, column, or diagonal) is the winner. If there is no
winner by the time the board is completely full of pieces, the game is declared
a "Cat's game" and it is a draw.
