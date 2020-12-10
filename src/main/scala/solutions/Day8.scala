package solutions

import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {
  val filename = "day8.txt"
  val file = Source.fromResource(filename)
  val instructions = file.getLines.toList.map { case s"${cmd} ${value}" => Instruction(cmd, value.toInt) }
  file.close()

  @tailrec
  def solution1(next: Int, called: List[Int], acc: Int): Int =
    if (called.contains(next)) acc
    else {
      val instruction = instructions(next)
      val (newAcc, nextInstruction) = instruction match {
        case Instruction("nop", _) => (acc, next + 1)
        case Instruction("acc", value) => (acc + value, next + 1)
        case Instruction("jmp", value) => (acc, next + value)
      }

      solution1(nextInstruction, called :+ next, newAcc)
    }

  def solution2(): Int = {
    @tailrec
    def validation(instructions: List[Instruction], next: Int, called: List[Int], acc: Int): Int = {
      if (next == instructions.size) {println("hi"); acc}
      else if (next < 0 || next > instructions.size) 0
      else if (called.contains(next)) 0
      else {
        val instruction = instructions(next)
        val (newAcc, nextInstruction) = instruction match {
          case Instruction("nop", _) => (acc, next + 1)
          case Instruction("acc", value) => (acc + value, next + 1)
          case Instruction("jmp", value) => (acc, next + value)
        }

        validation(instructions, nextInstruction, called :+ next, newAcc)
      }
    }

    instructions.zipWithIndex.foldRight(0) { case ((instruction, idx), acc) =>
      if (acc > 0) acc
      else instruction match {
        case Instruction("jmp", value) if value <= 0 => validation((instructions.take(idx) :+ Instruction("nop", value)) ++ instructions.takeRight(instructions.size - idx - 1), 0, List.empty, 0)
        case Instruction("nop", value) if value > 0 => validation((instructions.take(idx) :+ Instruction("jmp", value)) ++ instructions.takeRight(instructions.size - idx - 1), 0, List.empty, 0)
        case _ => 0
      }
    }
  }

  println(solution1(0, List.empty, 0))
  println(solution2())
}

final case class Instruction(command: String, value: Int)
