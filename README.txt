This project is a basic implementation for a Game of Life world. The
initialization state is read from a file following standard game of life
world format.  Further information about the world's format can be found
here:
==TODO== IRENE's link for the format description ==TODO==

The program can operate in two modes: with or without aging. With aging each
live cell from in the initialization state starts with age 1 and in every
iteration this age is increased by one if the live cell stays alive. A world
has a maximum possible age which is the upper bound for all the cells' age in
the given world. Exceeding this bound the cell turns into a dead cell.

The rest of the rules applied on the dead and live cells are identical to the
rules described on the Wikipedia's Game of Life page:
  http://en.wikipedia.org/wiki/Conway's_Game_of_Life


Usage:
The program has one mandatory parameter, the first parameter, which is the
location of the file describing the initialization world, and an optional
parameter, the second parameter, that states if the aging feature is set.

For instance, the following command runs the program without aging:
  ./Main patterns/pic135.life

While this command runs the same game with aging:
  ./Main patterns/pic135.life aging
