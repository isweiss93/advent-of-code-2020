package solutions

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {
  val filename = "day14.txt"
  val file = Source.fromResource(filename)
  val instructions = file.getLines.toList
  file.close()

  @tailrec
  def binary(num: Long, bits: List[Long]): List[Long] = {
    if (num == 0) bits
    else {
      @tailrec
      def largestBit(n: Long, b: Long): Long =
        if (b * 2 > n) b
        else largestBit(n, b * 2)

      val bit = largestBit(num, 1)

      binary(num - bit, bits :+ bit)
    }
  }

  def maskBits(mask: String): Map[Long, Boolean] =
    mask
      .zipWithIndex
      .flatMap { case (m, i) =>
        if (m == 'X') None
        else if (m == '1') Some(math.pow(2, 35 - i).toLong -> true)
        else Some(math.pow(2, 35 - i).toLong -> false)
      }
      .toMap

  def maskBitsWithChar(mask: String): Map[Long, Char] =
    mask
      .zipWithIndex
      .map { case (m, i) => math.pow(2, 35 - i).toLong -> m }
      .toMap

  def solution1(): Long = {
    val (_, memory) = instructions.foldLeft((Map.empty[Long, Boolean], Map.empty[Long, Long])) { case ((mask, mem), instruction) =>
      instruction match {
        case s"mask = ${m}" => (maskBits(m), mem)
        case s"mem[${address}] = ${value}" =>
          val longValue = value.toLong
          val bin = binary(longValue, List.empty)
          val bitsToAdd = mask.filter(_._2).keys.toList.filterNot(bin.contains)
          val bitsToSub = mask.filterNot(_._2).keys.toList.filter(bin.contains)

          (mask, mem + (address.toLong -> (longValue + bitsToAdd.sum - bitsToSub.sum)))
        case _ => (mask, mem)
      }
    }

    memory.values.sum
  }

  def solution2(): Long = {
    val (_, memory) = instructions.foldLeft((Map.empty[Long, Char], Map.empty[Long, Long])) { case ((mask, mem), instruction) =>
      instruction match {
        case s"mask = ${m}" => (maskBitsWithChar(m), mem)
        case s"mem[${address}] = ${value}" =>
          val longAddress = address.toLong
          val bin = binary(longAddress, List.empty)
          val bitsToAdd = mask.filter(_._2 == '1').keys.toList.filterNot(bin.contains)
          val bitsToSub = mask.filter(_._2 == 'X').keys.toList.filter(bin.contains)

          val xBits = mask.filter(_._2 == 'X').keys.toList

          val newMap = xBits.flatMap(b => List(b, 0)).combinations(xBits.length).foldLeft(mem) { case (old, bits) =>
            old + ((longAddress + bitsToAdd.sum - bitsToSub.sum + bits.sum) -> value.toLong)
          }

          (mask, newMap)
        case _ => (mask, mem)
      }
    }

    memory.values.sum
  }

  println(solution1())
  println(solution2())
}
