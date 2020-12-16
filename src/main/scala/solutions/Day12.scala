package solutions

import scala.io.Source

object Day12 extends App {
  val filename = "day12.txt"
  val file = Source.fromResource(filename)

  val regex = "([NSEWFLR])([0-9]+)".r
  val directions = file.getLines.toList.map { case regex(tpe, value) => Direction(tpe.head, value.toInt) }

  file.close()

  def turnShip(curr: Int, turn: Direction): Int = (curr + (if (turn.tpe == 'L') 360 - turn.value else turn.value)) % 360

  def rotateWaypoint(waypoint: Position, turn: Direction): Position = {
    val degrees = math.toDegrees(math.atan2(waypoint.y, waypoint.x))
    val r = math.hypot(waypoint.x, waypoint.y)
    val radians = math.toRadians((degrees + (if (turn.tpe == 'R') 360 - turn.value else turn.value)) % 360)

    Position(math.round(r * math.cos(radians)).toInt, math.round(r * math.sin(radians)).toInt)
  }

  def updateMap(map: Map[Char, Int], key: Char, value: Int): Map[Char, Int] =
    map + (key -> (map.getOrElse(key, 0) + value))

  def solution1(): Int = {
    val (_, turnMap) = directions
      .filter {
        case Direction('F', _) | Direction('R', _) | Direction('L', _) => true
        case _ => false
      }
      .foldLeft((90, Map.empty[Char, Int])) { case ((curr, map), Direction(tpe, value)) =>
        if (tpe == 'F') {
          (
            curr,
            if (curr == 0) updateMap(map, 'N', value)
            else if (curr == 90) updateMap(map, 'E', value)
            else if (curr == 180) updateMap(map, 'S', value)
            else updateMap(map, 'W', value)
          )
        }
        else (turnShip(curr, Direction(tpe, value)), map)
      }

    val totalMap = directions
      .groupBy(_.tpe)
      .map { case (tpe, dir) => (tpe, dir.map(_.value).sum) }

    val combinedMap = turnMap ++ totalMap.map { case (k, v) => k -> (v + turnMap.getOrElse(k, 0)) }

    val (northSouth, eastWest) = (combinedMap.getOrElse('N', 0) - combinedMap.getOrElse('S', 0),
      combinedMap.getOrElse('E', 0) - combinedMap.getOrElse('W', 0))

    Math.abs(northSouth) + Math.abs(eastWest)
  }

  def solution2(): Int = {
    val (_, ship) = directions.foldLeft((Position(10, 1), Position(0, 0))) { case ((waypoint, ship), dir) =>
      dir match {
        case Direction('N', value) => (Position(waypoint.x, waypoint.y + value), ship)
        case Direction('E', value) => (Position(waypoint.x + value, waypoint.y), ship)
        case Direction('S', value) => (Position(waypoint.x, waypoint.y - value), ship)
        case Direction('W', value) => (Position(waypoint.x - value, waypoint.y), ship)
        case Direction('F', value) => (waypoint, Position(ship.x + waypoint.x * value, ship.y + waypoint.y * value))
        case _ => (rotateWaypoint(waypoint, dir), ship)
      }
    }

    Math.abs(ship.x) + Math.abs(ship.y)
  }

  println(solution1())
  println(solution2())
}

final case class Direction(tpe: Char, value: Int)

final case class Position(x: Int, y: Int)
