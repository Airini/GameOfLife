# Lab 4: Game of Life  [ Team 18 ]

This project is a basic implementation for a Game of Life world. The
initialization state is read from a file using the Life 1.05 standard game of
life world format as a base (http://www.conwaylife.com/wiki/Life_1.05).

Some variations of this standard are made:
- normal rules are always assumed, so any line specifying alternate rules
  (those starting with "#R") are ignored along with the rest of description
  lines
- rows in the blocks to be put in the world can be specified in the plain,
  ordinary way (cell by cell) or by parameterising with numbers how many
  consecutive cells have the same state; examples would be:
  · plain row: ...*..*
  · parameterised row: 2.4*25.*.*7.2.
    equivalent to: [2 dead, 4 alive, 25 dead, 1 alive, 1 dead, 1 alive,
                    7 dead, 2 dead]
  both formats can be combined in any single row described in the file


The program can operate in two modes: with or without aging. With aging each
live cell from in the initialization state starts with age 1 and in every
iteration this age is increased by one if the live cell stays alive. A world
has a maximum possible age which is the upper bound for all the cells' ages
in the given world. Exceeding this bound the cell turns into a dead cell.

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

Furthermore, the running program is capable of two modifications on the camera
view: (1) positioning the camera, and (2) zooming. The former can be controlled
by the arrow buttons on the keyboard, while the latter is controlled by the
'+' and '-' buttons. The camera position is set to the middle of the world by
default, but in case of a large world zooming and positioning to a certain
part of the world might be handy.

** Differences from the initial proposal **

Because of the difficulties in using the gnuplot (actually, several of them)
interface modules, we decided to switch to OpenGL for graphical visualisation.

On the other hand, instead of extending to toroidal worlds, we decided to
first implement the age colour-coding and didn't get to implement the toroidal
world extension.

