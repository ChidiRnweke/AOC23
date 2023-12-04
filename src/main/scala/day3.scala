// scala-cli directives
//> using resourceDir ../resources
//> using file utils/utilities.scala
//> using dep org.typelevel::cats-parse::1.0.0
//> using dep org.typelevel::cats-core::2.10.0

import utils.readFile
import cats.parse.Numbers.{digits}
import cats.parse.Rfc5234.{crlf, alpha}
import cats.parse.Parser
import cats.parse.Caret
import cats.parse.Parser0

case class Symbol(value: String, r: Int, c: Int)
case class Nums(value: Int, adjacent: List[Symbol])

object Nums:
  def fromRowColumn(
      rawValue: String,
      row: Int,
      col: Int,
      symbols: List[Symbol]
  ) =
    val colRange = (col - rawValue.length() + 1 to col)

    def isAdjacent(col: Int)(r: Int, c: Int) =
      (row == r && math.abs(col - c) == 1)
        || (col == c && math.abs(row - r) == 1)

    def isAdjacentAny(r: Int, c: Int) =
      colRange.exists(isAdjacent(_)(r, c))

    val isDiagonal = (r: Int, c: Int) =>
      colRange.exists(c_ => math.abs(c - c_) == 1 && math.abs(row - r) == 1)

    val h = symbols.filter { case Symbol(_, r, c) => isAdjacentAny(r, c) }
    val diag = symbols.filter { case Symbol(_, r, c) => isDiagonal(r, c) }
    Nums(rawValue.toInt, h ::: diag)

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

  private val symbol =
    Parser.anyChar.filter(c => !c.isDigit && c != '.' && c != '\n').string

  val symbols = (symbol ~ Parser.caret).surroundedBy(dotsDigitsOrNewline).rep0

@main def day3: Unit =
  import Symbol._
  import ParseInput._
  val example = """467..114..
                  |...*......
                  |..35..633.
                  |......#...
                  |617*......
                  |.....+.58.
                  |..592.....
                  |......755.
                  |...$.*....
                  |.664.598..""".stripMargin

  val input = readFile("day3.txt")
    .getOrElse(throw new Exception("Failed to read file"))

  val symbols = symbolsFromParseResult(input)
  val solution1 = numberParser
    .parseAll(input)
    .getOrElse(throw new Exception("failed to parse"))
    .map((c, caret) => Nums.fromRowColumn(c, caret.line, caret.col, symbols))
    .filter(_.adjacent.nonEmpty)
    .map(_.value)
    .sum

  println(solution1)
