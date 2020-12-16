package solutions

import scala.io.Source

object Day15 extends App {
  val filename = "day15.txt"
  val file = Source.fromResource(filename)
  val nums = file.getLines.next().split(",").map(_.toInt).toList
  file.close()

  def solution(end: Int): Int = {
    val map = nums.init.zipWithIndex.map { case (n, i) => (n, i + 1) }.toMap

    val (lastNum, _) = nums.length.until(end).foldLeft((nums.last, map)) { case ((last, m), i) =>
      val mostRecentSighting = m.getOrElse(last, 0)

      if (mostRecentSighting == 0) (0, m + (last -> i))
      else (i - mostRecentSighting, m + (last -> i))
    }

    lastNum
  }

  println(solution(2020))
  println(solution(30000000))
}
