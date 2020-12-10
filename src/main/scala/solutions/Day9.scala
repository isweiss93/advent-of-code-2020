package solutions

import scala.io.Source

object Day9 extends App {
  val filename = "day9.txt"
  val file = Source.fromResource(filename)
  val numbers = file.getLines.toList.map(_.toLong)
  file.close()

  def solution1(preambleLength: Int): Long = {
    val queue = numbers.take(preambleLength)
    val list = numbers.drop(preambleLength)

    val (_, result) = list.foldLeft((queue, 0L)) { case ((q, badNum), next) =>
      if (badNum > 0) (q, badNum)
      else if (q.combinations(2).toList.map(_.sum).contains(next)) (q.tail :+ next, badNum)
      else (q.tail :+ next, next)
    }

    result
  }

  def solution2(invalid: Long): Long =
    (for {
      i <- numbers.indices
      j <- numbers.drop(i + 1).indices
    } yield {
      val subList = numbers.slice(i, j + 1)
      if (subList.sum == invalid) subList.min + subList.max
      else 0
    }).max

  val invalid = solution1(25)

  println(invalid)
  println(solution2(invalid))
}
