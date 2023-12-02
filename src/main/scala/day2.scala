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
    val num = stripped.takeWhile(_.isDigit).toInt
    stripped match
      case c if c.contains("red")   => Red(num)
      case c if c.contains("blue")  => Blue(num)
      case c if c.contains("green") => Green(num)

case class Game(id: Int, r: Seq[Red], g: Seq[Green], b: Seq[Blue]):
  val isPossible: Boolean =
    this match
      case Game(_, r, g, b)
          if r.forall(_.n < 13) && g.forall(_.n < 14) && b.forall(_.n < 15) =>
        true
      case _ => false

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
