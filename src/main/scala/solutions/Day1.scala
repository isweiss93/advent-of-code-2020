package solutions

import scala.io.Source

object Day1Puzzle1 {
  def main(args: Array[String]): Unit = {
    val filename = "day1.txt"
    val file = Source.fromResource(filename)
    val expenseReport = file.getLines().toList.map(_.toInt)
    file.close()

    val result = (for {
      (i, index) <- expenseReport.zipWithIndex
      j <- expenseReport.takeRight(expenseReport.length + 1 - index)
    } yield {
      if (i + j == 2020) {
        Some(i * j)
      }
      else None
    }).flatten.head

    println(result)
  }
}

object Day1Puzzle2 {
  def main(args: Array[String]): Unit = {
    val filename = "day1.txt"
    val file = Source.fromResource(filename)
    val expenseReport = file.getLines().toList.map(_.toInt)
    file.close()

    val result = (for {
      (i, index) <- expenseReport.zipWithIndex
      (j, jIndex) <- expenseReport.takeRight(expenseReport.length + 1 - index).zipWithIndex
      k <- expenseReport.takeRight(expenseReport.length + 1 - jIndex)
    } yield {
      if (i + j + k == 2020) {
        Some(i * j * k)
      }
      else None
    }).flatten.head

    println(result)
  }
}
