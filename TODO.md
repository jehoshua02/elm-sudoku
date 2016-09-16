# TODONE

+ Possible.eliminateUsed
+ Possible.eliminateCrowds
+ Possible.eliminateSame
+ Possible.eliminateAligned
+ Run elm-format validate in travis
+ Puzzle.solve

# TODOING

+ more tests for Puzzle.solve
  + get a sudoku book or find somewhere online that has puzzles with solutions

# TODO

+ Puzzle.solve guess and check
  + when no possibilities can be eliminated
  + and puzzle still not solved
  + find a space with only two possible
    + if no space with only two possible
    + Err Unsolvable
  + try first possible (set and recurse)
  + if first is unsolvable, try second possible (set and recurse)
  + if Err Unsolvable, return Err Unsolvable
  + if Ok puzzle, return Ok puzzle
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
