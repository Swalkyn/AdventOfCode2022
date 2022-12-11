import scala.io.Source

type Rope = List[Coord]

// val input = "R 4,U 4,L 3,D 1,R 4,D 1,L 5,R 2".split(",").toSeq
val input = Source.fromFile("src/main/resources/day9/input.txt").getLines().toSeq

sealed trait Direction { def incr: Coord }
case object U extends Direction { val incr = Coord(0, 1) }
case object D extends Direction { val incr = Coord(0, -1) }
case object L extends Direction { val incr = Coord(-1, 0) }
case object R extends Direction { val incr = Coord(1, 0) }

case class Coord(x: Int, y: Int):
  infix def +(other: Coord) = Coord(this.x + other.x, this.y + other.y)
  infix def -(other: Coord) = Coord(this.x - other.x, this.y - other.y)
  def abs = Coord(Math.abs(x), Math.abs(y))
  def MHDist(other: Coord) = Math.abs(this.x - other.x) + Math.abs(this.y - other.y)
  def DDist(other: Coord) = Math.max(Math.abs(this.x - other.x), Math.abs(this.y - other.y))
case class Motion(d: Direction, n: Int)

def moveTail(h: Coord, t: Coord): Coord =
  val d = t.DDist(h)
  if d < 2 then t
  else
    (for {
      x <- h.x-1 to h.x+1
      y <- h.y-1 to h.y+1
    } yield Coord(x, y))
      .filter(_.DDist(t) == 1)
      .minBy(_.MHDist(h))

def moveRopeOnce(r: Rope, incr: Coord): Rope =
  r.tail.scanLeft(r.head + incr)(moveTail).toList

def moveRope(r: Rope, m: Motion, acc: Set[Coord] = Set()): (Rope, Set[Coord]) =
  if m.n == 0 then
    (r, acc)
  else
    val newRope = moveRopeOnce(r, m.d.incr)
    moveRope(newRope, Motion(m.d, m.n - 1), acc + newRope.last)

def simulateRope(rope: Rope, motions: Seq[Motion]): Int =
  motions.foldLeft(rope, Set[Coord]()) { case ((rope, positions), motion) =>
    val (newRope, newPos) = moveRope(rope, motion)
    (newRope, positions ++ newPos)
  }._2.size

val rope1 = List.fill(2)(Coord(0, 0))
val rope2 = List.fill(10)(Coord(0, 0))
val motions = input.map {
  case s"L $n" => Motion(L, n.toInt)
  case s"R $n" => Motion(R, n.toInt)
  case s"U $n" => Motion(U, n.toInt)
  case s"D $n" => Motion(D, n.toInt)
}

// Part 1
simulateRope(rope1, motions)

// Part 2
simulateRope(rope2, motions)
