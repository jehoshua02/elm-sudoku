# TODONE

+ Possible.eliminateUsed
+ Possible.eliminateCrowds
+ Possible.eliminateSame
+ Possible.eliminateAligned
+ Run elm-format validate in travis

# TODOING

+ Puzzle.solve
  + return if puzzle solved
  + eliminate possibilities
  + if none eliminated, guess and check
    + unsolvable error if guess and check fails
  + recurse

# TODO

+ Puzzle.make
  + empty puzzle
  + a few random numbers
  + check if solvable
+ adhere to elm-package requirements/conventions (as an exercise, not necessarily to publish)
+ alternate puzzle sizes
  + update puzzle and possible types to contain dimension meta data
  + update all function to rely on puzzle/possible meta data
+ refactor
  + deeply nested, complex methods
+ sudoku server
+ sudoku client
