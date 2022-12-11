import scala.io.Source

val input = Source.fromFile("src/main/resources/day5/input.txt").getLines().toSeq
val stepPattern = "move (\\d+) from (\\d+) to (\\d+)".r

val (numberedLayout, stepsWithBlank) = input.span(!_.isEmpty)
val steps = stepsWithBlank.tail
val layout = numberedLayout.init

def parseStacks(table: Seq[String]) =
  table.transpose.tail.sliding(1, 4).flatten.map(_.dropWhile(_ == ' ')).toSeq

def moveCrates(oneByOne: Boolean)(currentLayout: Seq[Seq[Char]], step: String): Seq[Seq[Char]] =
  step match {
    case stepPattern(quantityStr, fromStr, toStr) => 
      val quantity = quantityStr.toInt
      val from = fromStr.toInt - 1
      val to = toStr.toInt - 1
      val crates = currentLayout(from).take(quantity)
      val ordered = if oneByOne then crates.reverse else crates
      currentLayout
        .updated(from, currentLayout(from).drop(quantity))
        .updated(to, ordered ++ currentLayout(to))
    case _ => throw IllegalArgumentException("bad format")
  }

val stacks = parseStacks(layout)

// Part 1
steps.foldLeft(stacks)(moveCrates(true)).map(_.head).mkString

// Part 2
steps.foldLeft(stacks)(moveCrates(false)).map(_.head).mkString
