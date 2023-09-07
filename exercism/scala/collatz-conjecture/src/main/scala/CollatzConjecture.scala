object CollatzConjecture {
  def steps(n: Int): Option[Int] = {
    def loop(n: Int, acc: Int): Option[Int] = {
      n match {
        case 1 => Some(acc)
        case n if n < 1 => None
        case n if n % 2 == 0 => loop(n / 2, acc + 1)
        case n => loop(3 * n + 1, acc + 1)
      }
    }
    loop(n, 0)
  }
}
