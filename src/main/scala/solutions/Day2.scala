package solutions

import scala.io.Source

object Day2Puzzle1 {
  def main(args: Array[String]): Unit = {
    val filename = "day2.txt"
    val file = Source.fromResource(filename)
    val passwords = file.getLines().toList
    file.close()

    val regex = "^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)".r
    val result = passwords.count { line =>
      val regex(min, max, letter, pass) = line
      val hist = pass.groupBy(c => c).map { case (c, list) => (c, list.length) }
      hist.getOrElse(letter.head, 0) >= min.toInt && hist(letter.head) <= max.toInt
    }

    println(result)
  }
}

object Day2Puzzle2 {
  def main(args: Array[String]): Unit = {
    val filename = "day2.txt"
    val file = Source.fromResource(filename)
    val passwords = file.getLines().toList
    file.close()

    val regex = "^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)".r
    val result = passwords.count { line =>
      val regex(pos1, pos2, letter, pass) = line
      pass.charAt(pos1.toInt - 1) == letter.head ^ pass.charAt(pos2.toInt - 1) == letter.head
    }

    println(result)
  }
}
