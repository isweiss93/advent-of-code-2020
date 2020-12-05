package solutions

import scala.io.Source

object Day1 extends App {
  val filename = "day1.txt"
  val file = Source.fromResource(filename)
  val expenseReport = file.getLines().toList.map(_.toInt)
  file.close()

  def solution1(): Int = (for {
    (i, index) <- expenseReport.zipWithIndex
    j <- expenseReport.takeRight(expenseReport.length + 1 - index)
  } yield {
    if (i + j == 2020) {
      Some(i * j)
    }
    else None
  }).flatten.head


  def solution2(): Int = (for {
    (i, index) <- expenseReport.zipWithIndex
    (j, jIndex) <- expenseReport.takeRight(expenseReport.length + 1 - index).zipWithIndex
    k <- expenseReport.takeRight(expenseReport.length + 1 - jIndex)
  } yield {
    if (i + j + k == 2020) {
      Some(i * j * k)
    }
    else None
  }).flatten.head

  println(solution1())
  println(solution2())
}
