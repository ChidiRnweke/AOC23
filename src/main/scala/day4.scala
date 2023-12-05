//> using resourceDir ../resources
//> using file utils/utilities.scala
//> using dep org.typelevel::cats-parse::1.0.0
//> using dep org.typelevel::cats-core::2.10.0
package day4

import cats.syntax.all._
import cats.Semigroup
import utils.readFile
import cats.parse.Numbers.{digits}
import cats.parse.Parser
import cats.parse.Rfc5234.{wsp, alpha, crlf}

case class Hand(as: List[Int])
case class Match(left: Hand, right: Hand):
  val matches = left.as.intersect(right.as).length
  val points = if matches == 0 then 0 else math.pow(2, matches - 1)

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

def duplicateCards(seen: Map[Int, Int], cards: (Match, Int)): Map[Int, Int] =
  val (round, cardnr) = cards
  val timesSeen = seen.get(cardnr + 1)
  val won = (cardnr + 2 to round.matches + cardnr + 1).toList

  val result = timesSeen match
    case None        => cardnr + 1 :: won
    case Some(value) => cardnr + 1 :: List.fill(value + 1)(won).flatten

  val resultMap = result.groupBy(identity).view.mapValues(_.size).toMap
  Semigroup[Map[Int, Int]].combine(seen, resultMap)

@main def day4: Unit =

  val input = readFile("day4.txt")
    .getOrElse(throw new Exception("Failed to read file"))

  val parsed = ParseInput.parseGame
    .parseAll(input)
    .getOrElse(throw new Exception("Parsing failed"))

  val solution1 = parsed.map(_.points).sum

  val solution2 =
    parsed.zipWithIndex.foldLeft(Map.empty)(duplicateCards).values.sum

  println(solution1)
  println(solution2)
