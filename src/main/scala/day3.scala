// scala-cli directives
//> using resourceDir ../resources
//> using file utils/utilities.scala
//> using dep org.typelevel::cats-parse::1.0.0
//> using dep org.typelevel::cats-core::2.10.0
package day3

import utils.readFile
import cats.parse.Numbers.{digits}
import cats.parse.Parser
import Nums._

case class Symbol(value: Char, r: Int, c: Int)
case class Nums(value: Int, adjacent: List[Symbol])

object Nums:
  import ParseInput._
  def fromRowColumn(
      rawInput: String,
      row: Int,
      col: Int,
      symbols: List[Symbol]
  ) =
    val colRange = (col - rawInput.length() + 1 to col)

    def isAdjacent(col: Int)(r: Int, c: Int): Boolean =
      (row == r && math.abs(col - c) == 1)
        || (col == c && math.abs(row - r) == 1)

    def isAdjacentAny(r: Int, c: Int): Boolean =
      colRange.exists(isAdjacent(_)(r, c))

    def isDiagonal(r: Int, c: Int): Boolean =
      colRange.exists(c_ => math.abs(c - c_) == 1 && math.abs(row - r) == 1)

    val h = symbols.filter { case Symbol(_, r, c) => isAdjacentAny(r, c) }
    val diag = symbols.filter { case Symbol(_, r, c) => isDiagonal(r, c) }
    Nums(rawInput.toInt, (h ::: diag).distinct)

  def rawInputToNums(rawInput: String, symbols: List[Symbol]) =
    numberParser
      .parseAll(rawInput)
      .getOrElse(throw new Exception("failed to parse"))
      .map((c, caret) => Nums.fromRowColumn(c, caret.line, caret.col, symbols))
      .filter(_.adjacent.nonEmpty)

object Symbol:
  def symbolsFromParseResult(input: String): List[Symbol] =
    ParseInput.symbols
      .parseAll(input)
      .getOrElse(throw new Exception("failed to parse"))
      .map((c, caret) => Symbol(c, caret.line, caret.col))

object ParseInput:
  private val nums = digits ~ Parser.caret
  private val notNums = Parser.anyChar.filter(!_.isDigit).backtrack.rep0
  val numberParser = nums.surroundedBy(notNums).rep

  private val dots = Parser.char('.').rep
  private val newLine = Parser.char('\n')
  private val dotsDigitsOrNewline = (dots | digits | newLine).rep0
  private val symbolPattern = (c: Char) => !c.isDigit && c != '.' && c != '\n'
  private val symbol = Parser.anyChar.filter(symbolPattern)

  val symbols = (symbol ~ Parser.caret).surroundedBy(dotsDigitsOrNewline).rep0

@main def day3: Unit =
  import Symbol._

  val input = readFile("day3.txt")
    .getOrElse(throw new Exception("Failed to read file"))

  val symbols = symbolsFromParseResult(input)
  val solution1 = rawInputToNums(input, symbols).map(_.value).sum

  val gears = symbols.filter(_.value == '*')
  val solution2 =
    rawInputToNums(input, gears)
      .groupBy(_.adjacent)
      .map(_._2)
      .filter(_.length >= 2)
      .map(_.foldRight(1)(_.value * _))
      .sum

  println(solution1)
  println(solution2)
