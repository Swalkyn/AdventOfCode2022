import scala.io.Source

type Coord = (Int, Int)

val input = Source.fromFile("src/main/resources/day08/input.txt").getLines().toSeq
// val input = """30373,25512,65332,33549,35390""".trim.split(",").toSeq

val grid: Map[Coord, Int] = input
  .map(_.map(_.asDigit).toSeq)
  .zipWithIndex
  .flatMap((row, i) => row.zipWithIndex.map((h, j) => (j, i) -> h))
  .toMap
val width = input.head.size
val height = input.size

assert(width == height)
val n = width

// Part 1
val range = (0 until n)
range.flatMap(i =>
  range.foldLeft(-1, Set[(Int, Int)]()) {
    case ((max, acc), j) => if grid(i, j) > max then (grid(i, j), acc incl (i, j)) else (max, acc)
  }._2 ++
  range.foldRight(-1, Set[(Int, Int)]()) {
    case (j, (max, acc)) => if grid(i, j) > max then (grid(i, j), acc incl (i, j)) else (max, acc)
  }._2 ++
  range.foldLeft(-1, Set[(Int, Int)]()) {
    case ((max, acc), j) => if grid(j, i) > max then (grid(j, i), acc incl (j, i)) else (max, acc)
  }._2 ++
  range.foldRight(-1, Set[(Int, Int)]()) {
    case (j, (max, acc)) => if grid(j, i) > max then (grid(j, i), acc incl (j, i)) else (max, acc)
  }._2
).toSet.size

def scenicScore(c: Coord) =
  val (x, y) = c
  val h = grid(x, y)
  if (x == 0 || x == width-1 || y == 0 || y == height-1) then
    0
  else
    val right = LazyList.range(x+1, width-1, 1).map(x => grid(x, y)).takeWhile(_ < h).size + 1
    val left = LazyList.range(x-1, 0, -1).map(x => grid(x, y)).takeWhile(_ < h).size + 1
    val up = LazyList.range(y+1, height-1, 1).map(y => grid(x, y)).takeWhile(_ < h).size + 1
    val down = LazyList.range(y-1, 0, -1).map(y => grid(x, y)).takeWhile(_ < h).size + 1
    right * left * up * down

// Part 2
grid.keys.map(scenicScore).max

