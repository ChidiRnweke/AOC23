// scala-cli directives
//> using resourceDir ../resources
//> using file utils/utilities.scala
package day1
import utils.readFile

def part1Solution(rawInput: String): Int =
  rawInput
    .split("\n")
    .map(_.filter(_.isDigit))
    .map(line => (line.head.asDigit * 10) + line.last.asDigit)
    .sum

def part2Solution(rawInput: String): Int =

  def mapDigits(line: String): String =
    line
      .replaceAll("one", "o1e")
      .replaceAll("two", "t2o")
      .replaceAll("three", "t3e")
      .replaceAll("four", "f4r")
      .replaceAll("five", "f5e")
      .replaceAll("six", "s6x")
      .replaceAll("seven", "s7n")
      .replaceAll("eight", "e8t")
      .replaceAll("nine", "n9e")

  part1Solution(mapDigits(rawInput))

@main def day1: Unit =
  val example = """1abc2
                  |pqr3stu8vwx
                  |a1b2c3d4e5f
                  |treb7uchet""".stripMargin

  val example2 = """two1nine
                    |eightwothree
                    |abcone2threexyz
                    |xtwone3four
                    |4nineeightseven2
                    |zoneight234
                    |7pqrstsixteen""".stripMargin

  val input = readFile("day1.txt")
    .getOrElse(throw new Exception("Failed to read file"))

  println(part1Solution(input))
  println(part2Solution((input)))
