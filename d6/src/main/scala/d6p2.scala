import d6p1.*
object d6p2 extends Solution[BigInt]:

  def parseWithPrefix(prefix: String, input: String): BigInt =
    input.split(" ").toList.filter(!_.isBlank()) match
      case prefix :: values => BigInt(values.reduce(_ + _))
      case _                => 0

  def parseTime(input: String): BigInt     = parseWithPrefix("Time:", input)
  def parseDistance(input: String): BigInt = parseWithPrefix("Distance:", input)

  def parseRound(input: List[String]): Round =
    Round(parseTime(input.head), parseDistance(input.last))

  override def solve(input: List[String]): BigInt =
    calculateWinningAttemps(parseRound(input)).size
