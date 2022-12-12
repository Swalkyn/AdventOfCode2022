import scala.io.Source
import scala.collection.mutable

sealed trait Direction { def incr: Coord }
case object U extends Direction { val incr = Coord(0, 1) }
case object D extends Direction { val incr = Coord(0, -1) }
case object L extends Direction { val incr = Coord(-1, 0) }
case object R extends Direction { val incr = Coord(1, 0) }

case class Coord(x: Int, y: Int):
  infix def +(other: Coord) = Coord(this.x + other.x, this.y + other.y)
  infix def -(other: Coord) = Coord(this.x - other.x, this.y - other.y)
  def abs = Coord(Math.abs(x), Math.abs(y))

val input = Source.fromFile("src/main/resources/day12/input.txt").getLines().toSeq

val grid = (for {
  (row, y) <- input.zipWithIndex
  (cell, x) <- row.zipWithIndex
} yield Coord(x, y) -> cell).toMap

val start = grid.find(_._2 == 'S').get._1
val end = grid.find(_._2 == 'E').get._1

val grid2 = grid
  .updated(start, 'a')
  .updated(end, 'z')
  .view.mapValues(c => c.toInt - 'a'.toInt)

def shortestPath(from: Coord, to: Coord): Int =
  val distances = mutable.Map[Coord, Int]().withDefaultValue(Integer.MAX_VALUE)
  val unexplored = mutable.Set[Coord]().addAll(grid.keys)
  distances(from) = 0

  while (!unexplored.isEmpty)
    val u = unexplored.minBy(distances(_))
    unexplored.remove(u)

    List(U, D, L, R)
      .map(_.incr + u)
      .filter(v => grid2.get(v).filter(_ - grid2(u) <= 1).isDefined && unexplored.contains(v))
      .foreach(neighbour =>
        val dist = distances(u) + 1
        if dist < distances(neighbour) then
          distances(neighbour) = dist
      )
  distances(to)

shortestPath(start, end)


