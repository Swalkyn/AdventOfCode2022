import scala.io.Source
import scala.collection.mutable.Queue

class Monkey(
  val index: Int,
  val items: Queue[Int],
  val op: Int => Int,
  val divisor: Int,
  val trueIndex: Int,
  val falseIndex: Int
):
  var monkeyMap: Map[Int, Monkey] = Map()
  var inspections: Int = 0

  override def toString = s"Monkey $index"

  def receiverMonkeyFor(item: Int) =
    if item % divisor == 0 then monkeyMap(trueIndex) else monkeyMap(falseIndex)

  def examineItem(item: Int) =
    val newItem = op(item) / 3
    receiverMonkeyFor(newItem).items.enqueue(newItem)
    inspections += 1

  def examineAll = items.dequeueAll(_ => true).foreach(examineItem)

val endingNumberPattern = "(\\d+)$".r.unanchored
def lastInt(s: String): Int = s match 
  case endingNumberPattern(n) => n.toInt
  case _ => 0

def parseMonkeyOp(s: String): Int => Int = s match
  case s"$a + $b" => old => parseMonkeyOp(a)(old) + parseMonkeyOp(b)(old)
  case s"$a * $b" => old => parseMonkeyOp(a)(old) * parseMonkeyOp(b)(old)
  case "old" => (old: Int) => old
  case n => old => n.toInt

def parseMonkeyInfo(lines: Seq[String]) = lines match
  case Seq(s"Monkey $n:", s"  Starting items: $items", s"  Operation: new = $op", test, caseTrue, caseFalse) =>
    val queue = Queue(items.split(", ").map(_.toInt):_*)
    Monkey(n.toInt, queue, parseMonkeyOp(op), lastInt(test), lastInt(caseTrue), lastInt(caseFalse))
  case _ => throw IllegalArgumentException()

def monkeyBusiness(monkeys: Seq[Monkey]) = monkeys.map(_.inspections).sorted(Ordering[Int].reverse).take(2).product

val input = Source.fromFile("src/main/resources/day11/input.txt").getLines().toSeq

def part1(input: Seq[String]): Int =
  val monkeys = input.sliding(6, 7).map(parseMonkeyInfo).toList
  val monkeyMap = monkeys.map(m => (m.index, m)).toMap
  monkeys.foreach(m => m.monkeyMap = monkeyMap)
  (0 until 20).foreach(_ => monkeys.foreach(_.examineAll))
  monkeyBusiness(monkeys)

println(part1(input))

