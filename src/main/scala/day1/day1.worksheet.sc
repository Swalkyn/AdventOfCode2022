import scala.math.Ordering
import scala.io.Source

val input = Source.fromFile("src/main/resources/day1/input.txt").getLines().toList

val totalByElf = input.map(_.toIntOption).foldRight((0, List[Int]())) {
  case (Some(cal), (current, acc))  => (current + cal, acc)
  case (None, (current, acc)) => (0, current :: acc)
}._2

val answer1 = totalByElf.max
val answer2 = totalByElf.sorted(Ordering[Int].reverse).take(3).sum

