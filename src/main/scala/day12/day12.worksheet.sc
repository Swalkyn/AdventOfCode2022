import scala.io.Source
import scala.collection.mutable

sealed trait Direction { def incr: Coord }
object Direction { def all = List(U, D, L, R)}
case object U extends Direction { val incr = Coord(0, 1) }
case object D extends Direction { val incr = Coord(0, -1) }
case object L extends Direction { val incr = Coord(-1, 0) }
case object R extends Direction { val incr = Coord(1, 0) }

case class Coord(x: Int, y: Int):
  infix def +(other: Coord) = Coord(this.x + other.x, this.y + other.y)

val input = Source.fromFile("src/main/resources/day12/input.txt").getLines().toSeq

val gridWithSE = (for {
  (row, y) <- input.zipWithIndex
  (cell, x) <- row.zipWithIndex
} yield Coord(x, y) -> cell).toMap

val start = gridWithSE.find(_._2 == 'S').get._1
val end = gridWithSE.find(_._2 == 'E').get._1
val grid = gridWithSE
  .updated(start, 'a')
  .updated(end, 'z')
  .view.mapValues(c => c.toInt - 'a'.toInt)

// Dijsktra, but we do not have to check for smaller distances, since all edges have weight 1
def shortestPath(from: Coord, to: Coord => Boolean, canMove: (Coord, Coord) => Boolean): Map[Coord, Int] =
  val distances = mutable.Map[Coord, Int]().withDefaultValue(10000)
  val unexplored = mutable.Set[Coord]().addAll(grid.keys)
  distances(from) = 0

  while (!unexplored.isEmpty)
    val u = unexplored.minBy(distances(_))
    unexplored.remove(u)
    Direction.all
      .map(_.incr + u)
      .filter(v => grid.contains(v) && canMove(u, v) && unexplored.contains(v))
      .foreach(neighbour => distances(neighbour) = distances(u) + 1)
  distances.filter((c, _) => to(c)).toMap

// Part 1
shortestPath(start, c => c == end, (u, v) => grid(v) - grid(u) <= 1).head

// Part 2
shortestPath(end, c => grid(c) == 0, (u, v) => grid(u) - grid(v) <= 1).minBy(_._2)

