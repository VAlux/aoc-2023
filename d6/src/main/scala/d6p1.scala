object d6p1 extends Solution[Int]:
  case class Round(time: Int, distance: Int)

  def parseWithPrefix(prefix: String, input: String): List[Int] =
    input.split(" ").toList.filter(!_.isBlank()) match
      case prefix :: values => values.map(_.toInt)
      case _                => List.empty

  def parseTime(input: String): List[Int]     = parseWithPrefix("Time:", input)
  def parseDistance(input: String): List[Int] = parseWithPrefix("Distance:", input)

  def parseRounds(input: List[String]): List[Round] =
    parseTime(input.head).zip(parseDistance(input.last)).map(Round.apply)

  def calculateWinningAttemps(round: Round): List[Round] =
    (0 to round.time)
      .map(speed => Round(speed, speed * (round.time - speed)))
      .filter(_.distance > round.distance)
      .toList

  override def solve(input: List[String]): Int =
    parseRounds(input).map(calculateWinningAttemps).map(_.size).reduce(_ * _)
