// scala-cli directives
//> using resourceDir ../resources
//> using file utils/utilities.scala

import utils.readFile
import Color._

enum Color:
  case Red(n: Int)
  case Green(n: Int)
  case Blue(n: Int)

object Color:
  def fromStripped(stripped: String): Color =
    stripped match
      case s"${n}blue"  => Blue(n.toInt)
      case s"${n}green" => Green(n.toInt)
      case s"${n}red"   => Red(n.toInt)

case class Game(id: Int, r: Seq[Red], g: Seq[Green], b: Seq[Blue]):
  val isPossible: Boolean =
    r.forall(_.n < 13) && b.forall(_.n < 15) && g.forall(_.n < 14)

  val minimumColors = (r.maxBy(_.n).n, g.maxBy(_.n).n, b.maxBy(_.n).n)

object Game:
  def fromColorArray(colors: Seq[Color], id: Int): Game =
    val reds = colors.collect { case r: Red => r }
    val greens = colors.collect { case g: Green => g }
    val blues = colors.collect { case b: Blue => b }
    Game(id, reds, greens, blues)

  def fromRaw(rawInput: String): Iterator[Game] =
    rawInput.linesIterator
      .map(_.drop(7))
      .map(_.replaceAll(":| ", ""))
      .map(_.split(",|;"))
      .map(_.map(Color.fromStripped(_)))
      .zipWithIndex
      .map((colors, id) => Game.fromColorArray(colors, id + 1))

def part1Solution(rawInput: String): Int =
  Game
    .fromRaw(rawInput)
    .filter(_.isPossible)
    .map(_.id)
    .sum

def part2Solution(rawInput: String): Int =
  Game
    .fromRaw(rawInput)
    .map(_.minimumColors)
    .map(_ * _ * _)
    .sum

@main def day2: Unit =
  val input = readFile("day2.txt")
    .getOrElse(throw new Exception("Failed to read file"))

  println(part1Solution(input))
  println(part2Solution(input))
