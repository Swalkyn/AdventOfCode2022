import scala.io.Source

val input = Source.fromFile("src/main/resources/day6/input.txt").getLines().toSeq

// Fast to write, slightly inefficient soltion
def findFirstNDistinct(s: String, n: Int) =
  s.sliding(n).zipWithIndex.find((chars, _) => chars.distinct.size == n).get._2 + n

// Part 1
findFirstNDistinct(input.head, 4)

// Part 2
findFirstNDistinct(input.head, 14)

