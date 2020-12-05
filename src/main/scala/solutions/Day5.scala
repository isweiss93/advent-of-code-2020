package solutions

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {
  val filename = "day5.txt"
  val file = Source.fromResource(filename)

  val regex = "([FB]{7})([LR]{3})".r

  val boardingPasses = file.getLines().toList.map { case regex(forwardBack, leftRight) => BoardingPass(forwardBack, leftRight) }
  file.close()

  def toIds(): List[Int] = {
    @tailrec
    def binarySearch(lowerBound: Int, upperBound: Int, commands: String): Int = {
      if (lowerBound == upperBound) lowerBound
      else {
        val midPoint = (lowerBound + upperBound) / 2
        val (newLowerBound, newUpperBound) = commands.head match {
          case 'F' | 'L' => (lowerBound, midPoint)
          case 'B' | 'R' => (midPoint + 1, upperBound)
        }

        binarySearch(newLowerBound, newUpperBound, commands.tail)
      }
    }

    boardingPasses.map { boardingPass =>
      binarySearch(0, 127, boardingPass.forwardBack) * 8 +
        binarySearch(0, 7, boardingPass.leftRight)
    }
  }

  val boardingIds = toIds()

  def solution1(): Int = boardingIds.max

  def solution2(): Int = {
    boardingIds.min.to(boardingIds.max).diff(boardingIds).head
  }

  println(solution1())
  println(solution2())
}

final case class BoardingPass(forwardBack: String, leftRight: String)
