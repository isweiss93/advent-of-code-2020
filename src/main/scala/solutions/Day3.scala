package solutions

import scala.annotation.tailrec
import scala.io.Source

object Day3Puzzle1 {
  def main(args: Array[String]): Unit = {
    val filename = "day3.txt"
    val file = Source.fromResource(filename)
    val trees = file.getLines().toList.map(_.toList)
    val tree = '#'
    file.close()

    val bottomBound = trees.length - 1
    val rightBound = trees.head.length

    @tailrec
    def checkNextPos(x: Int, y: Int, total: Int): Int = {
      if (y == bottomBound) total
      else {
        val nextX = (x + 3) % rightBound
        val nextY = y + 1
        val isTree = if (trees(nextY)(nextX) == tree) 1 else 0
        val newTotal = total + isTree

        checkNextPos(nextX, nextY, newTotal)
      }
    }

    println(checkNextPos(0, 0, 0))

  }
}

object Day3Puzzle2 {
  def main(args: Array[String]): Unit = {
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

    val slopes = Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    val result = slopes.map { case (xInc, yInc) => checkNextPos(0, 0, xInc, yInc, 0) }.product

    println(result)

  }
}
