# TODONE

+ Possible.eliminateUsed
+ Possible.eliminateCrowds
+ Possible.eliminateSame
+ Possible.eliminateAligned
+ Run elm-format validate in travis
+ Puzzle.solve

# TODOING

+ diagnose and fix Possible.eliminateCrowds
  + it's wreaking havoc on the puzzle, modifying answers
  + should not touch any place there is only one possible
  + reenable eliminateCrowds in Puzzle.solve

# TODO

+ more tests for Puzzle.solve
+ don't mix errors from different methods
  + fromList: InvalidLength | OutOfRange
  + solve: Unsolvable
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
