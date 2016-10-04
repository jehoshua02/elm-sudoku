var _jehoshua02$elm_sudoku$Native_Random = function() {

function percentage()
{
  return Math.random();
}

function int(min, max)
{
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

return {
  percentage: percentage,
  int: F2(int),
};

}();
