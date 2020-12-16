package solutions

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {
  val filename = "day13.txt"
  val file = Source.fromResource(filename)
  val input = file.getLines.toList
  file.close()

  val earliestDeparture = input.head.toInt
  val buses = input(1).split(",")

  @tailrec
  def findClosestArrival(bus: Int, time: Int, target: Int): Int =
    if (time >= target) time
    else findClosestArrival(bus, time + bus, target)

  def solution1(): Int = {
    val (bus, arrival) = buses
      .filter(_ != "x")
      .map { bus =>
        (bus.toInt, findClosestArrival(bus.toInt, 0, earliestDeparture))
      }
      .minBy(_._2)

    bus * (arrival - earliestDeparture)
  }

  def solution2(): Double = {
    val congruence = buses.zipWithIndex.filter(_._1 != "x").map { case (bus, i) => (bus.toLong, bus.toLong - i) }.toList

    val m = congruence.map(_._1).product
    val yValues = congruence.map { case (bus, _) => (bus, m / bus) }

    @tailrec
    def moduloInverse(a: Long, m: Long, b: Long): Long = {
      if ((a * b) % m == 1) b
      else moduloInverse(a, m, b + 1)
    }

    val zValues = yValues.map { case (bus, y) => moduloInverse(y % bus, bus, 1)}

    yValues.map(_._2).zip(zValues).zip(congruence.map(_._2)).map { case ((y, z), a) => a * y * z }.sum % m
  }

  println(solution1())
  println(solution2().toLong)
}
