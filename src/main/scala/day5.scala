//> using resourceDir ../resources
//> using file utils/utilities.scala
//> using dep org.typelevel::cats-parse::1.0.0
//> using dep org.typelevel::cats-core::2.10.0
package day5

import cats._
import cats.data._
import cats.syntax.all._

import utils.readFile
import cats.parse.Numbers.{digits}
import cats.parse.Parser
import cats.parse.Rfc5234.{wsp, alpha, lf, digit}

object InputParser:
  private val titleParser = Parser.anyChar.repUntil(digits)
  private val nl = Parser.char('\n').?.void
  private val numbers = digits.repSep(wsp).repSep(nl)
  val inputParser = (titleParser *> numbers).rep

  def rawToParsed(rawString: String) =
    val parsed = inputParser
      .parseAll(rawString)
      .getOrElse(throw Exception("Failed to parse"))

    val seeds = parsed.head
@main def day4: Unit =
  val example = """seeds: 79 14 55 13

                  |seed-to-soil map:
                  |50 98 2
                  |52 50 48

                  |soil-to-fertilizer map:
                  |0 15 37
                  |37 52 2
                  |39 0 15

                  |fertilizer-to-water map:
                  |49 53 8
                  |0 11 42
                  |42 0 7
                  |57 7 4

                  |water-to-light map:
                  |88 18 7
                  |18 25 70

                  |light-to-temperature map:
                  |45 77 23
                  |81 45 19
                  |68 64 13

                  |temperature-to-humidity map:
                  |0 69 1
                  |1 0 69

                  |humidity-to-location map:
                  |60 56 37
                  |56 93 4""".stripMargin

  val input = readFile("day5.txt")
    .getOrElse(throw new Exception("Failed to read file"))
  println(InputParser.inputParser.parseAll(example))
