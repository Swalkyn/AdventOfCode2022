package day2

import scala.io.Source
import scala.util.matching.Regex

object Day2 extends App:
  sealed abstract class Shape:
    val score: Int
    val beats: Shape
    val loses: Shape

  case object Rock extends Shape:
    val score = 1
    val beats = Scissors 
    val loses = Paper

  case object Paper extends Shape:
    val score = 2
    val beats = Rock 
    val loses = Scissors

  case object Scissors extends Shape:
    val score = 3
    val beats = Paper 
    val loses = Rock

  val input = Source.fromFile("src/main/resources/day2/input.txt").getLines().toList

  val pattern = "(.) (.)".r

  // p1 is the opponent, p2 is you
  val p1Encoding = Map("A" -> Rock, "B" -> Paper, "C" -> Scissors)
  val p2Encoding = Map("X" -> Rock, "Y" -> Paper, "Z" -> Scissors)

  def getScore = (p1: Shape, p2: Shape) => (p2, p1) match
    case (Rock, Rock) => 3
    case (Rock, Paper) => 0
    case (Rock, Scissors) => 6
    case (Paper, Rock) => 6
    case (Paper, Paper) => 3
    case (Paper, Scissors) => 0
    case (Scissors, Rock) => 0
    case (Scissors, Paper) => 6
    case (Scissors, Scissors) => 3

  def getScoreFromLetters(p1Letter: String, p2Letter: String) =
    val p2Shape = p2Encoding(p2Letter)
    getScore(p1Encoding(p1Letter), p2Shape) + p2Shape.score

  def getScoreFromLetters2(a: String, b: String) =
    val p1Shape = p1Encoding(a)
    val p2Shape = b match
      case "X" => p1Shape.beats
      case "Y" => p1Shape
      case "Z" => p1Shape.loses
    getScore(p1Shape, p2Shape) + p2Shape.score

  def part1 = input
    .map(_ match
      case pattern(p1, p2) => (p1, p2))
    .map(getScoreFromLetters)
    .sum

  def part2 = input
    .map(_ match
      case pattern(p1, p2) => (p1, p2))
    .map(getScoreFromLetters2)
    .sum

  println(part1)
  println(part2)


