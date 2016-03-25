# ScHex
Hex board game in Scala
See https://en.wikipedia.org/wiki/Hex_(board_game) for details on the game

## Run

> scalac hex.scala

> scala hex

Optional argument -s SIZE can be used to create boards of different sizes.
Defaults to 14 by 14.


## PLAY

Type :q to quit  
Type two integers separated by space in order to record a play

## Testing

Tested the game with two StdInPlayers by running through multiple rounds.

## Implementation

HexBoard is a trait that keeps track of cell states, the list of neighbors of
each cell, and knows how to turn itself into a string

The HexGame extends HexBoard.  It also knows about the hex game itself by:
  - Using the HexBoard trait to keep track of cell states,
  - Knowing which player is supposed to play next by incrementing a counter called iteration,
  - Calling play() on a Player
  - Determining whether a player has won

The HexGame takes two Player instances in its constructor.
Player is an abstract class that defines a play() method.  The play() method
takes an Int tuple (row, column) with the last move taken by the adversary and
must return a valid (row, column) tuple representing a new play.

Each Player implementation is responsible for keeping track of the board state.

I provide one concrete implementation of Player called the StdInPlayer,
which reads a move from standard input.

I started working on an AiPlayer that extends Player, but haven't gotten to
a place where it is usable yet.  My idea was to keep track of what I call 
"connected components," which are all the cells that belong to either a 
player or its opponent that are adjacent to each other.  The AI would use this
information to find the shortest path between two connected components.
Since the shortest path calculation is expensive, it would only be done up 
to a max depth.

A player would play pseudo randomly until one of two things happened:
  1. It determine that its opponent could win the game in defendDepth plays
  2. It determine that some of its connected components are in attackDepthdistance from each other.

If one of these conditions were satisfied, the plays would be targeted at 
blocking the opponent from winning (in the case of 1), or connecting its own
components (in the case of 2).

In order to increase the chances of forming connected components, the pseudo 
random play would favor parts of the board over others.
