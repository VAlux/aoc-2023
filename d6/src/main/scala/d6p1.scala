object d6p1 extends Solution[BigInt]:
  case class Round(time: BigInt, distance: BigInt)

  def parseWithPrefix(prefix: String, input: String): List[BigInt] =
    input.split(" ").toList.filter(!_.isBlank()) match
      case prefix :: values => values.map(BigInt(_))
      case _                => List.empty

  def parseTime(input: String): List[BigInt]     = parseWithPrefix("Time:", input)
  def parseDistance(input: String): List[BigInt] = parseWithPrefix("Distance:", input)

  def parseRounds(input: List[String]): List[Round] =
    parseTime(input.head).zip(parseDistance(input.last)).map(Round.apply)

  def calculateWinningAttemps(round: Round): List[Round] =
    (BigInt(0) to round.time)
      .map(speed => Round(speed, speed * (round.time - speed)))
      .filter(_.distance > round.distance)
      .toList

  override def solve(input: List[String]): BigInt =
    parseRounds(input).map(calculateWinningAttemps).map(_.size).reduce(_ * _)
