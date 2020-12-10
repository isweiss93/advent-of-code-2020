package solutions

import scala.collection.mutable
import scala.io.Source

object Day10 extends App {
  val filename = "day10.txt"
  val file = Source.fromResource(filename)
  val adapters = file.getLines.toList.map(_.toInt).sorted
  file.close()

  def solution1(): Int =
    (0 :: adapters).zip(adapters :+ (adapters.max + 3))
      .map { case (lower, higher) => higher - lower }
      .groupBy(d => d)
      .filter { case (diff, _) => diff == 1 || diff == 3 }
      .values
      .map(_.size)
      .product

  def solution2(): Long = {
    val newAdapters = adapters :+ (adapters.max + 3)

    def chainAdapters(joltage: Int, memo: mutable.Map[Int, Long]): Long = {
      if (joltage == 0) 1
      else if (!newAdapters.contains(joltage) || joltage < 0) 0
      else if (memo.contains(joltage)) memo(joltage)
      else {
        memo.put(joltage, chainAdapters(joltage - 1, memo) + chainAdapters(joltage - 2, memo) + chainAdapters(joltage - 3, memo))
        memo(joltage)
      }
    }

    chainAdapters(newAdapters.max, mutable.Map.empty)
  }


  println(solution1())
  println(solution2())
}
