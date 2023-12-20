object d4p1 extends Solution[Int]:

  case class Card(id: Int, winning: Set[Int], actual: Set[Int]):
    override def toString(): String =
      s"winning: [${winning.mkString(",")}] actual: [${actual.mkString(",")}]"

  def extractScoringNumbers(card: Card): Set[Int] =
    card.winning.intersect(card.actual)

  def calculateScore(card: Card): Int =
    Math.pow(2, extractScoringNumbers(card).size - 1).toInt

  def parseCard(input: String): Option[Card] =
    def parseNumbers(numbers: String): Set[Int] =
      numbers.split(" ").filter(!_.isBlank()).map(_.toInt).toSet

    input.split(":") match
      case Array(name, numbers) =>
        val id = name.split(" ").last.toInt
        numbers.trim.split("\\|") match
          case Array(winning, actual) => Some(Card(id, parseNumbers(winning), parseNumbers(actual)))
          case _                      => None
      case _                    => None

  override def solve(input: List[String]): Int =
    input.flatMap(parseCard).map(calculateScore).sum
