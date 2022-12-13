import scala.io.Source

extension (l: String)
  infix def ord(r: String): Boolean = (l.head, r.head) match
    case (lh, rh) if lh == rh => l.tail ord r.tail
    case (']', _)  => true
    case (_, ']')  => false
    case ('[', rh) => l.tail ord s"$rh]${r.tail}"
    case (lh, '[') => s"$lh]${l.tail}" ord r.tail
    case (lh, rh)  => lh < rh
 
val input = Source.fromFile("src/main/resources/day13/example.txt").getLines().toSeq
val packets = input.collect { case s if !s.isEmpty => s.replace("10", "A") }

// Part 1
packets
  .grouped(2)
  .zipWithIndex
  .collect { case ((l :: r :: Nil), i) if l ord r => i + 1 }
  .sum

val divPackets = Seq("[[2]]", "[[6]]")

// Part 2
(packets ++ divPackets)
  .sortWith(_ ord _)
  .zipWithIndex
  .collect { case (p, i) if divPackets.contains(p) => i + 1 }
  .product
