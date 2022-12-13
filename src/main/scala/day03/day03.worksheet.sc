import scala.io.Source

val input = Source.fromFile("src/main/resources/day03/input.txt").getLines().toList

def commonItems(rucksack: String): Set[Char] =
  val (compA, compB) = rucksack.splitAt(rucksack.size / 2)
  compA.toSet.intersect(compB.toSet)

def commonGroupItems(rucksacks: List[String]): Set[Char] =
  rucksacks.map(_.toSet).reduce(_.intersect(_))

def itemPriority(c: Char): Int =
  if c.isLower then c.toInt - 'a'.toInt + 1
  else c.toInt - 'A'.toInt + 27

// Part 1
input.flatMap(commonItems).map(itemPriority).sum

// Part 2
input.sliding(3, 3).flatMap(commonGroupItems).map(itemPriority).sum
