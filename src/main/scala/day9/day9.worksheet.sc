import scala.io.Source

// val input = "R 4,U 4,L 3,D 1,R 4,D 1,L 5,R 2".split(",").toSeq
val input = Source.fromFile("src/main/resources/day9/input.txt").getLines().toSeq

sealed trait Direction { def incr: Coord }
case object U extends Direction { val incr = Coord(0, 1) }
case object D extends Direction { val incr = Coord(0, -1) }
case object L extends Direction { val incr = Coord(-1, 0) }
case object R extends Direction { val incr = Coord(1, 0) }

case class Coord(x: Int, y: Int):
  infix def +(other: Coord) = Coord(this.x + other.x, this.y + other.y)
  def distanceTo(other: Coord) = Math.abs(this.x - other.x) + Math.abs(this.y - other.y)
case class Motion(d: Direction, n: Int)

def moveRopeEnds(h: Coord, t: Coord, m: Motion, acc: Set[Coord] = Set()): (Coord, Coord, Set[Coord]) =
  if m.n == 0 then
    (h, t, acc)
  else
    val newH = h + m.d.incr
    val d = t.distanceTo(newH)
    val newT = if d < 2 || (d == 2 && t.x != newH.x && t.y != newH.y) then t else h
    moveRopeEnds(newH, newT, Motion(m.d, m.n - 1), acc + newT)

moveRopeEnds(Coord(4, 0), Coord(3, 0), Motion(U, 2))

input
  .map {
    case s"L $n" => Motion(L, n.toInt)
    case s"R $n" => Motion(R, n.toInt)
    case s"U $n" => Motion(U, n.toInt)
    case s"D $n" => Motion(D, n.toInt)
  }
  .foldLeft((Coord(0, 0), Coord(0, 0), Set[Coord]())) { case ((h, t, positions), motion) =>
    val (newH, newT, newPos) = moveRopeEnds(h, t, motion)
    (newH, newT, positions ++ newPos)
  }._3.size
