object d2p1 extends Solution:
  enum CubeType(val name: String):
    case RED   extends CubeType("red")
    case GREEN extends CubeType("green")
    case BLUE  extends CubeType("blue")

  object CubeType:
    def fromName(name: String): Option[CubeType] = CubeType.values.find(_.name == name)

  case class RoundEntry(cubeType: CubeType, amount: Int)
  case class Round(entries: Set[RoundEntry])
  case class Game(id: Int, rounds: List[Round])

  val bagContents = Map(
    CubeType.RED   -> 12,
    CubeType.GREEN -> 13,
    CubeType.BLUE  -> 14
  )

  def parseGame(input: String): Option[Game] =
    def parseRoundEntry(entry: String): Option[RoundEntry] =
      entry.split(" ") match
        case Array(amount, color) => CubeType.fromName(color).map(RoundEntry(_, amount.toInt))
        case _                    => None

    def parseRound(definition: String): Round =
      Round(definition.split(",").map(_.trim()).flatMap(parseRoundEntry).toSet)

    input.split(":") match
      case Array(gameIdDefinition, roundsDefinition) =>
        val id     = gameIdDefinition.split(" ")(1)
        val rounds = roundsDefinition.split(";").map(parseRound).toList
        Some(Game(id.toInt, rounds))
      case _                                         => None

  def possibleRound(round: Round): Boolean =
    def possibleRoundEntry(entry: RoundEntry): Boolean = bagContents(entry.cubeType) >= entry.amount
    round.entries.forall(possibleRoundEntry)

  override def solve(input: List[String]): Int =
    input
      .flatMap(parseGame)
      .filter(_.rounds.forall(possibleRound))
      .map(_.id)
      .sum
