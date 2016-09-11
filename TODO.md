# TODONE

+ Possible.eliminateUsed
+ Possible.eliminateCrowds

# TODOING

+ cleanup
  + try to get rid of private helper functions

# TODO

+ Possible.eliminateSame
  + n locations, same n possibilities, live in same row/column/group
  + eliminate those possibilities in row/column/group
  + need to check all groups, rows, columns independently
+ Possible.eliminateAligned
  + within a group, all possibilities for a value exist in same row/column
  + eliminate possibility for that number in other groups sharing row/column
+ Puzzle.solve
  + calculate possible
  + fill locations with only one possible
  + if none to fill, need to figure out how to guess and check
  + recurse
+ Puzzle.make
+ adhere to elm-package requirements/conventions (as an exercise, not necessarily to publish)
+ alternate puzzle sizes
+ Run elm-format validate in travis
