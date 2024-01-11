import d2p1.*
object d2p2 extends Solution[Int]:

  def groupRoundsByCubeType(rounds: List[Round]): Map[CubeType, List[RoundEntry]] =
    rounds.flatMap(_.entries).groupBy(entry => entry.cubeType)

  def findMaxAmountCubeType(mapping: Map[CubeType, List[RoundEntry]]): Map[CubeType, Int] =
    mapping.map((cubeType, rounds) => cubeType -> rounds.maxBy(round => round.amount).amount)

  def getGamePower(aggregation: Map[CubeType, Int]): Int =
    aggregation.values.reduce(_ * _)

  override def solve(input: List[String]): Int =
    input
      .flatMap(d2p1.parseGame)
      .map(game => groupRoundsByCubeType(game.rounds))
      .map(findMaxAmountCubeType)
      .map(getGamePower)
      .sum
