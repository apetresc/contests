object Bearing extends Enumeration {
  val North = Value(0)
  val East = Value(1)
  val South = Value(2)
  val West = Value(3)
}

case class Robot(bearing: Bearing.Value, coordinates: (Int, Int)) {
  def turnRight: Robot = this.copy(bearing = Bearing.apply((bearing.id + 1) % 4))
  def turnLeft: Robot = this.copy(bearing = Bearing.apply((bearing.id + 3) % 4))

  def advance: Robot = {
    val (x, y) = coordinates
    bearing match {
      case Bearing.North => this.copy(coordinates = (x, y + 1))
      case Bearing.East => this.copy(coordinates = (x + 1, y))
      case Bearing.South => this.copy(coordinates = (x, y - 1))
      case Bearing.West => this.copy(coordinates = (x - 1, y))
    }
  }

  def simulate(instructions: String): Robot = instructions.foldLeft(this) {
    case (robot, 'R') => robot.turnRight
    case (robot, 'L') => robot.turnLeft
    case (robot, 'A') => robot.advance
    case (_, instruction) => throw new IllegalArgumentException(s"Invalid instruction: $instruction")
  }
}
