// Bearing enum
object Bearing extends Enumeration {
  type Bearing = Value
  val North, East, South, West = Value
}

case class Robot(bearing: Bearing.Bearing, coordinates: (Int, Int)) {
  def turnRight: Robot = bearing match {
    case Bearing.North => this.copy(bearing = Bearing.East)
    case Bearing.East => this.copy(bearing = Bearing.South)
    case Bearing.South => this.copy(bearing = Bearing.West)
    case Bearing.West => this.copy(bearing = Bearing.North)
  }

  def turnLeft: Robot = bearing match {
    case Bearing.North => this.copy(bearing = Bearing.West)
    case Bearing.East => this.copy(bearing = Bearing.North)
    case Bearing.South => this.copy(bearing = Bearing.East)
    case Bearing.West => this.copy(bearing = Bearing.South)
  }

  def advance: Robot = bearing match {
    case Bearing.North => this.copy(coordinates = (coordinates._1, coordinates._2 + 1))
    case Bearing.East => this.copy(coordinates = (coordinates._1 + 1, coordinates._2))
    case Bearing.South => this.copy(coordinates = (coordinates._1, coordinates._2 - 1))
    case Bearing.West => this.copy(coordinates = (coordinates._1 - 1, coordinates._2))
  }

  def simulate(instructions: String): Robot = instructions.foldLeft(this) {
    case (robot, 'R') => robot.turnRight
    case (robot, 'L') => robot.turnLeft
    case (robot, 'A') => robot.advance
    case (_, instruction) => throw new IllegalArgumentException(s"Invalid instruction: $instruction")
  }
}
