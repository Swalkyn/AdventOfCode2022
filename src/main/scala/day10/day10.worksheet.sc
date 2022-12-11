import scala.io.Source

val input = Source.fromFile("src/main/resources/day10/input.txt").getLines().toSeq

val run = input.scanLeft((0, 1)){
  case ((cycle, x), "noop")     => (cycle + 1, x) 
  case ((cycle, x), s"addx $n") => (cycle + 2, x + n.toInt)
}

// Part 1
(20 to 220 by 40).foldLeft(0){ case (acc, cycle) => (acc + run.findLast(_._1 < cycle).get._2 * cycle) }

val screen = Seq.fill(40 * 6)('.')

def drawScreen(run: List[(Int, Int)], screen: Seq[Char] = screen): Seq[Char] = run match {
  case (c1, x) :: (c2, y) :: more =>
    val newScreen = (c1 until c2).foldLeft(screen)((screen, c) => if (x-1 to x+1).contains(c % 40) then screen.updated(c, '█') else screen)
    drawScreen((c2, y) :: more, newScreen)
  case _ => screen
}

// Part 2
"\n" ++ drawScreen(run.toList, screen).grouped(40).map(_.mkString).mkString("\n")
