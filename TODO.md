# DOING

# TODO

+ Sudoku.solve
  + different techniques for eliminating possibilities
    + used
      + most basic check
      + look at intersecting rows/columns/groups
      + eliminate numbers already used
    + obscured single possibility
      + only one place a number can go in row/column/group
      + not enough used numbers to eliminate other possiblities
      + cannot merely look at possibilities for one location to see it
    + same possibilities
      + n locations, same n possibilities, live in same row/column/group
      + eliminate those possibilities in row/column/group
      + need to check all groups, rows, columns independently
    + aligned possibilities
      + within a group, all possibilities for a value exist in same row/column
      + eliminate possibility for that number in other groups sharing row/column
    + single possibility
      + only one possibility
      + fill and recalculate possibilities
  + optimizing sequence of techniques
  + lots and lots of testing with actual puzzles with known solutions
+ Sudoku.make
+ adhere to elm-package requirements/conventions (as an exercise, not necessarily to publish)
+ alternate puzzle sizes
