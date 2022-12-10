import scala.io.Source

val input = Source.fromFile("src/main/resources/day10/input.txt").getLines().toSeq

val run = input.scanLeft((0, 1)){
  case ((cycle, x), "noop")     => (cycle + 1, x) 
  case ((cycle, x), s"addx $n") => (cycle + 2, x + n.toInt)
}

// Part 1
(20 to 220 by 40).foldLeft(0){ case (acc, cycle) => (acc + run.findLast(_._1 < cycle).get._2 * cycle) }

