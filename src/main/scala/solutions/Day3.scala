package solutions

import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {
  val filename = "day3.txt"
  val file = Source.fromResource(filename)
  val trees = file.getLines().toList.map(_.toList)
  val tree = '#'
  file.close()

  val bottomBound = trees.length - 1
  val rightBound = trees.head.length

  @tailrec
  def checkNextPos(x: Int, y: Int, xInc: Int, yInc: Int, total: Long): Long = {
    if (y == bottomBound) total
    else {
      val nextX = (x + xInc) % rightBound
      val nextY = y + yInc
      val isTree = if (trees(nextY)(nextX) == tree) 1 else 0
      val newTotal = total + isTree

      checkNextPos(nextX, nextY, xInc, yInc, newTotal)
    }
  }

  def solution1(): Long = checkNextPos(0, 0, 3, 1, 0)

  def solution2(): Long = {
    val slopes = Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    slopes.map { case (xInc, yInc) => checkNextPos(0, 0, xInc, yInc, 0) }.product
  }

  println(solution1())
  println(solution2())
}
