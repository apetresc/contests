object Yacht {

  def score(dices: List[Int], category: String): Int = {
    val frequencyMap = dices.groupBy(identity).view.mapValues(_.size).toMap

    category match {
      case "ones" => dices.filter(_ == 1).sum
      case "twos" => dices.filter(_ == 2).sum
      case "threes" => dices.filter(_ == 3).sum
      case "fours" => dices.filter(_ == 4).sum
      case "fives" => dices.filter(_ == 5).sum
      case "sixes" => dices.filter(_ == 6).sum
      case "full house" => if (frequencyMap.values.toList.sorted == List(2, 3)) dices.sum else 0
      case "four of a kind" => frequencyMap.find(_._2 >= 4).map(_._1 * 4).getOrElse(0)
      case "little straight" => if (dices.sorted == List(1, 2, 3, 4, 5)) 30 else 0
      case "big straight" => if (dices.sorted == List(2, 3, 4, 5, 6)) 30 else 0
      case "choice" => dices.sum
      case "yacht" => if (frequencyMap.values.toList == List(5)) 50 else 0
      case _ => 0
    }
  }
}
