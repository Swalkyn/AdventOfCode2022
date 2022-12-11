import scala.io.Source
import scala.collection.mutable.Queue

// Not too happy with this solution. It works and is fast,
// but is not totally pure. Also not the easiest to read.
// Better solution: https://github.com/maneatingape/advent-of-code/blob/main/src/main/scala/AdventOfCode2022/Day11.scala

class Monkey(
  val index: Int,
  val items: Queue[Long],
  val op: Long => Long,
  val divisor: Long,
  val trueIndex: Int,
  val falseIndex: Int
):
  var monkeyMap: Map[Int, Monkey] = Map()
  var worryCheck = (x: Long) => x / 3
  var inspections: Long = 0

  override def toString = s"Monkey $index"

  def receiverMonkeyFor(item: Long) =
    if item % divisor == 0 then monkeyMap(trueIndex) else monkeyMap(falseIndex)

  def examineItem(item: Long) =
    val newItem = worryCheck(op(item))
    assert(newItem > 0)
    receiverMonkeyFor(newItem).items.enqueue(newItem)
    inspections += 1

  def examineAll = items.dequeueAll(_ => true).foreach(examineItem)

val endingNumberPattern = "(\\d+)$".r.unanchored
def lastInt(s: String): Int = s match 
  case endingNumberPattern(n) => n.toInt
  case _ => 0

def parseMonkeyOp(s: String): Long => Long = s match
  case s"$a + $b" => old => parseMonkeyOp(a)(old) + parseMonkeyOp(b)(old)
  case s"$a * $b" => old => parseMonkeyOp(a)(old) * parseMonkeyOp(b)(old)
  case "old" => (old: Long) => old
  case n => old => n.toLong

def parseMonkeyInfo(lines: Seq[String]) = lines match
  case Seq(s"Monkey $n:", s"  Starting items: $items", s"  Operation: new = $op", test, caseTrue, caseFalse) =>
    val queue = Queue(items.split(", ").map(_.toLong):_*)
    Monkey(n.toInt, queue, parseMonkeyOp(op), lastInt(test), lastInt(caseTrue), lastInt(caseFalse))
  case _ => throw IllegalArgumentException()

def monkeyBusiness(monkeys: Seq[Monkey]) = monkeys.map(_.inspections).sorted(Ordering[Long].reverse).take(2).product

val input = Source.fromFile("src/main/resources/day11/input.txt").getLines().toSeq

def part1(input: Seq[String]): Long =
  val monkeys = input.sliding(6, 7).map(parseMonkeyInfo).toList
  val monkeyMap = monkeys.map(m => (m.index, m)).toMap
  monkeys.foreach(m => m.monkeyMap = monkeyMap)
  (0 until 20).foreach(_ => monkeys.foreach(_.examineAll))
  monkeyBusiness(monkeys)

def part2(input: Seq[String]): Long =
  val monkeys = input.sliding(6, 7).map(parseMonkeyInfo).toList
  val monkeyMap = monkeys.map(m => (m.index, m)).toMap
  val divisorsProduct = monkeys.map(_.divisor).product
  val worryCheck = (x: Long) => x % divisorsProduct
  monkeys.foreach(m => m.monkeyMap = monkeyMap)
  monkeys.foreach(m => m.worryCheck = worryCheck)
  (0 until 10000).foreach(_ => monkeys.foreach(_.examineAll))
  monkeyBusiness(monkeys)

println(part1(input))
println(part2(input))

