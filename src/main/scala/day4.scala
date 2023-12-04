// scala-cli directives
//> using resourceDir ../resources
//> using file utils/utilities.scala
//> using dep org.typelevel::cats-parse::1.0.0
//> using dep org.typelevel::cats-core::2.10.0
package day4

case class Hand(as: List[Int])
case class Match(left: Hand, right: Hand):
  private val matches = left.as.intersect(right.as).length
  val points = if matches == 0 then 0 else math.pow(2, matches - 1)

import utils.readFile
import cats.parse.Numbers.{digits}
import cats.parse.Parser
import cats.parse.Rfc5234.{wsp, alpha, crlf}
object ParseInput:
  private val card = alpha.rep ~ wsp.rep ~ digits ~ Parser.char(':')
  private val sep = Parser.char('|').void

  private val any = Parser.anyChar

  private val hand = digits
    .map(_.toInt)
    .surroundedBy(wsp.rep.?)
    .repUntil(sep)
    .map(c => Hand(c.toList))

  private val parseHand = hand.between(card, sep)

  val parseGame = (parseHand ~ hand).repSep(any).map(_.map(Match(_, _)))

@main def day4: Unit =

  val example = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
                  |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
                  |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
                  |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
                  |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
                  |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin

  val input = readFile("day4.txt")
    .getOrElse(throw new Exception("Failed to read file"))

  val parsed = ParseInput.parseGame
    .parseAll(input)
    .getOrElse(throw new Exception("Parsing failed"))
  val solution = parsed.map(_.points).toList.sum
  println(solution)
