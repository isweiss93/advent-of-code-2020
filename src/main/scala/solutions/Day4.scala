package solutions

import scala.io.Source

object Day4 extends App {
  val filename = "day4.txt"
  val file = Source.fromResource(filename)

  val regex = "(.*):(.*)".r

  val passports = file.getLines().toList
    .map(s => if (s.isEmpty) "\n" else s)
    .mkString(" ")
    .split("\n")
    .map(_.trim)
    .map(_.split(" ").map { case regex(key, value) => key -> value }.toMap)

  file.close()

  def solution1(): Int = passports.count { pass =>
    List("byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid").forall(pass.keys.toList.contains)
  }

  def solution2(): Int = {
    val hgtRegex = "([0-9]+)(cm|in)".r
    val hclRegex = "#[0-9a-f]{6}".r
    val eclRegex = "(amb|blu|brn|gry|grn|hzl|oth)".r
    val pidRegex = "[0-9]{9}".r

    def hgtRule(s: String): Boolean = {
      s match {
        case hgtRegex(hgt, unit) =>
          val h = hgt.toInt
          if (unit == "cm") h >= 150 && h <= 193 else h >= 59 && h <= 76
        case _ => false
      }
    }

    val rules = Map(
      "byr" -> ((s: String) => s.toInt >= 1920 && s.toInt <= 2002),
      "iyr" -> ((s: String) => s.toInt >= 2010 && s.toInt <= 2020),
      "eyr" -> ((s: String) => s.toInt >= 2020 && s.toInt <= 2030),
      "hgt" -> hgtRule _,
      "hcl" -> ((s: String) => hclRegex.pattern.matcher(s).matches),
      "ecl" -> ((s: String) => eclRegex.pattern.matcher(s).matches),
      "pid" -> ((s: String) => pidRegex.pattern.matcher(s).matches),
      "cid" -> ((_: String) => true)
    )

    passports.count { pass =>
      val containsAllFields =
        List("byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid").forall(pass.keys.toList.contains)

      val passesRules = pass.forall { case (k, v) => rules(k)(v) }

      containsAllFields && passesRules
    }
  }

  println(solution1())
  println(solution2())
}
