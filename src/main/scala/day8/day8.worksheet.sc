import scala.io.Source

val input = Source.fromFile("src/main/resources/day8/input.txt").getLines().toSeq
val testInput = """30373,25512,65332,33549,35390""".trim.split(",").toSeq

val grid = input.map(_.map(_.asDigit).toSeq)

val horizontally = grid.zipWithIndex.flatMap((row, i) =>
  val indexedRow = row.zipWithIndex
  val visibleFromLeft = indexedRow.foldLeft(-1, List[(Int, Int)]()) {
    case ((max, acc), (h, j)) => if h > max then (h, (i, j) :: acc) else (max, acc)
  }._2
  val visibleFromRight = indexedRow.foldRight(-1, List[(Int, Int)]()) {
    case ((h, j), (max, acc)) => if h > max then (h, (i, j) :: acc) else (max, acc)
  }._2
  visibleFromRight ++ visibleFromLeft
  )

val vertically = grid.transpose.zipWithIndex.flatMap((row, j) =>
  val indexedRow = row.zipWithIndex
  val visibleFromLeft = indexedRow.foldLeft(-1, List[(Int, Int)]()) {
    case ((max, acc), (h, i)) => if h > max then (h, (i, j) :: acc) else (max, acc)
  }._2
  val visibleFromRight = indexedRow.foldRight(-1, List[(Int, Int)]()) {
    case ((h, i), (max, acc)) => if h > max then (h, (i, j) :: acc) else (max, acc)
  }._2
  visibleFromRight ++ visibleFromLeft
  )

(horizontally ++ vertically).toSet.size

