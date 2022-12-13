import scala.io.Source

val input = Source.fromFile("src/main/resources/day04/input.txt").getLines().toList
val pattern = "(\\d+)-(\\d+),(\\d+)-(\\d+),?".r

def extractRanges(s: String): (Range, Range) =
  s match {
    case pattern(s1, e1, s2, e2) => (Range(s1.toInt, e1.toInt+1), Range(s2.toInt, e2.toInt+1))
    case _ => throw IllegalArgumentException("Format error")
  }

extension (r1: Range)
  def includes(r2: Range) = r1.contains(r2.start) && r1.contains(r2.end-1)
  def overlaps(r2: Range) = r1.contains(r2.start) || r1.contains(r2.end-1) || r2.contains(r1.start) || r2.contains(r1.end-1)

// Part 1
input.map(extractRanges).count((r1, r2) => r1.includes(r2) || r2.includes(r1))

// Part 2
input.map(extractRanges).count(_.overlaps(_))

