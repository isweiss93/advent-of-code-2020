package solutions

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {
  val filename = "day11.txt"
  val file = Source.fromResource(filename)
  val seats = file.getLines.toList.map(_.toList)
  file.close()

  def checkAdjacentSeats(seatMatrix: List[List[Char]])(i: Int, j: Int): IndexedSeq[Boolean] =
    for {
      k <- (i - 1).to(i + 1)
      l <- (j - 1).to(j + 1)
    } yield {
      if (k < 0 || k >= seatMatrix.size || l < 0 || l >= seatMatrix.head.size || (k == i && l == j)) false
      else if (seatMatrix(k)(l) != '#') false
      else true
    }

  def checkVisibleSeats(seatMatrix: List[List[Char]])(i: Int, j: Int): IndexedSeq[Boolean] =
    for {
      k <- (i - 1).to(i + 1)
      l <- (j - 1).to(j + 1)
    } yield {
      @tailrec
      def directionalCheck(multiplier: Int): Boolean = {
        val newK = (k - i) * multiplier + i
        val newL = (l - j) * multiplier + j

        if (newK < 0 || newK >= seatMatrix.size || newL < 0 || newL >= seatMatrix.head.size || (newK == i && newL == j)) false
        else if (seatMatrix(newK)(newL) == 'L') false
        else if (seatMatrix(newK)(newL) == '#') true
        else directionalCheck(multiplier + 1)
      }

      directionalCheck(1)
    }

  def compareMatrices(oldMatrix: List[List[Char]], newMatrix: List[List[Char]]): IndexedSeq[Boolean] =
    for {
      i <- oldMatrix.indices
      j <- oldMatrix.head.indices
    } yield {
      oldMatrix(i)(j) == newMatrix(i)(j)
    }

  def calculateNewMaxtrix(seatMatrix: List[List[Char]], checkFunc: (Int, Int) => IndexedSeq[Boolean], tolerance: Int): List[List[Char]] =
    seatMatrix.indices.map { i =>
      seatMatrix.head.indices.map { j =>
        val surroundingSeats = checkFunc(i, j)
        if (seatMatrix(i)(j) == 'L') {
          if (surroundingSeats.forall(!_)) '#'
          else 'L'
        }
        else if (seatMatrix(i)(j) == '#') {
          if (surroundingSeats.count(b => b) >= tolerance) 'L'
          else '#'
        }
        else '.'
      }.toList
    }.toList

  @tailrec
  def solution1(seatMatrix: List[List[Char]]): Int = {
    val newMatrix = calculateNewMaxtrix(seatMatrix, checkAdjacentSeats(seatMatrix), 4)

    if (compareMatrices(seatMatrix, newMatrix).forall(b => b)) seatMatrix.flatten.count(_ == '#')
    else solution1(newMatrix)
  }

  @tailrec
  def solution2(seatMatrix: List[List[Char]]): Int = {
    val newMatrix = calculateNewMaxtrix(seatMatrix, checkVisibleSeats(seatMatrix), 5)

    if (compareMatrices(seatMatrix, newMatrix).forall(b => b)) seatMatrix.flatten.count(_ == '#')
    else solution2(newMatrix)
  }

  println(solution1(seats))
  println(solution2(seats))
}

