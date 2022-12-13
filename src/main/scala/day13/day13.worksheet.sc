import scala.io.Source
import scala.annotation.tailrec

// Black magic necessary to define recursive types
type Rec[F[_], A] = A match {
  case Int => Int | F[Rec[F, Int]]
  case _ => A | F[Rec[F, A]]
}

type Value = Rec[List, Int]
type Packet = List[Value]

def parse(s: String): Packet =
  val iter = s.tail.iterator
  def parseToken(s: Iterator[Char], acc: Packet = Nil): Packet =
    if !s.hasNext then acc.reverse
    else s.next match
      case '[' => parseToken(s, parseToken(s, Nil) :: acc)
      case ']' => acc.reverse
      case ',' => parseToken(s, acc)
      case n   => parseToken(s, (n.toString + s.takeWhile(_.isDigit).mkString).toInt :: acc)
  parseToken(s.tail.iterator)

extension (l: Value)
  infix def <(r: Value): Boolean = (l, r) match
    case (l: Int, r: Int) => l < r
    case (Nil, Nil) => false
    case (_, Nil) => false
    case (Nil, _) => true
    case (lh :: lt, rh :: rt) =>
      if lh < rh then true
      else if rh < lh then false
      else lt < rt
    case (l: Int, r: List[Value]) => List(l) < r
    case (l: List[Value], r: Int) => l < List(r)
 
val input = Source.fromFile("src/main/resources/day13/input.txt").getLines().toSeq
val packets = input.filter(!_.isEmpty).map(parse)

// Part 1
packets
  .grouped(2)
  .zipWithIndex
  .collect { case ((l :: r :: Nil), i) if l < r => i + 1 }
  .sum

given ord: Ordering[Value] = Ordering.fromLessThan(_ < _)
val divPackets = Seq("[[2]]", "[[6]]").map(parse)

// Part 2
(packets ++ divPackets)
  .sorted
  .zipWithIndex
  .collect { case (p, i) if divPackets.contains(p) => i + 1 }
  .product
