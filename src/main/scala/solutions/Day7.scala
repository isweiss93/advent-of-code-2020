package solutions

import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App {
  val filename = "day7.txt"
  val file = Source.fromResource(filename)

  val regex = "^([0-9] )?([a-z ]+) (bag|bags).?$".r

  val bags = file.getLines.toList
    .map { case s"${outerColor} bags contain ${seq}" =>
      (
        outerColor,
        seq
          .split(", ")
          .collect { case regex(num, bagColor, _) if num != null => Bag(num.trim.toInt, bagColor) }
      )
    }
    .toMap

  file.close()

  @tailrec
  def solution1(targetBags: List[String], bags: Map[String, Array[Bag]], correctColors: List[String]): Int =
    if (targetBags.isEmpty) correctColors.distinct.size
    else {
      val newTargetBags = bags.filter(_._2.map(_.color).toList.intersect(targetBags).nonEmpty).keys.toList
      solution1(newTargetBags, bags, correctColors ++ newTargetBags)
    }

  def solution2(color: String): Int = {
    val innerBags = bags(color)

    def countBags(bagList: List[Bag]): Int =
      if (bagList.isEmpty) 0
      else bagList.map(b => b.number * countBags(bags(b.color).toList) + b.number).sum

    countBags(innerBags.toList)
  }

  println(solution1(List("shiny gold"), bags, List.empty))
  println(solution2("shiny gold"))
}

case class Bag(number: Int, color: String)
