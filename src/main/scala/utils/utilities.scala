package utils

import scala.util.{Try, Using}
import scala.io.Source.fromResource

def readFile(filename: String): Try[String] =
  Using(fromResource(filename))(_.mkString)
