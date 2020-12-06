package solutions

import scala.io.Source

object Day6 extends App {
  val filename = "day6.txt"
  val file = Source.fromResource(filename)
  val lines = file.getLines.toList
  file.close()

  def solution1(): Int = lines
    .foldLeft(List(Set.empty[Char])){ case (acc, str) =>
      if (str.nonEmpty) acc.init :+ acc.last ++ str.toList
      else acc :+ Set.empty
    }
    .map(_.size)
    .sum

  def solution2(): Int = lines
    .foldLeft(List((Map.empty[Char, Int], 0))){ case (acc, str) =>
      if (str.nonEmpty) acc.init :+ (acc.last match {
        case (map, cnt) => (str.foldLeft(Map.empty[Char, Int]){ case (a, c) =>
          a + (c -> (map.getOrElse(c, 0) + 1))
        }, cnt + 1)
      })
      else acc ++ List((Map.empty[Char, Int], 0))
    }.map{ case (map, cnt) => map.count{ case (_, c) => c == cnt } }
    .sum

  println(solution1())
  println(solution2())
}
