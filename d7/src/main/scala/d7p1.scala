import scala.util.chaining.*
object d7p1 extends Solution[Int]:

  val cardRanking = Map(
    'A' -> 14,
    'K' -> 13,
    'Q' -> 12,
    'J' -> 11,
    'T' -> 10,
    '9' -> 9,
    '8' -> 8,
    '7' -> 7,
    '6' -> 6,
    '5' -> 5,
    '4' -> 4,
    '3' -> 3,
    '2' -> 2,
    'J' -> 1
  )

  case class Card(name: Char, rank: Int)
  case class Hand(cards: List[Card], bid: Int):
    override def toString(): String =
      s"[${cards.map(_.name).mkString("][")}] - Bid: $bid"

  case class Round(hand: Hand, handType: HandType):
    override def toString(): String =
      s"Hand: $hand Type: $handType"

  def countOccurences(hand: Hand): Map[Card, Int] =
    hand.cards.groupMapReduce(identity)(_ => 1)(_ + _)

  object Round:
    given Ordering[Round] with
      override def compare(x: Round, y: Round): Int =
        val rankComparison = Ordering.Int.compare(x.handType.rank, y.handType.rank)
        if rankComparison != 0 then rankComparison
        else
          x.hand.cards
            .zip(y.hand.cards)
            .map((left, right) => (left.rank, right.rank))
            .find((left, right) => left - right != 0)
            .map(Ordering.Int.compare)
            .getOrElse(0)

  enum HandType(val rank: Int):
    case FiveOfAKind  extends HandType(7)
    case FourOfAKind  extends HandType(6)
    case FullHouse    extends HandType(5)
    case ThreeOfAKind extends HandType(4)
    case TwoPair      extends HandType(3)
    case OnePair      extends HandType(2)
    case HighCard     extends HandType(1)

  object HandType:
    val joker = parseCard('J')

    given Ordering[HandType] with
      override def compare(x: HandType, y: HandType): Int =
        Ordering.Int.compare(x.rank, y.rank)

    extension (a: Map[Card, Int])
      def containsAmount(n: Int): Boolean = a.values.exists(_ == n)
      def amountOf(n: Int): Int           = a.values.count(_ == n)

    def checkOccurences(occurences: Map[Card, Int]): HandType =
      if occurences.containsAmount(5) then HandType.FiveOfAKind
      else if occurences.containsAmount(4) then HandType.FourOfAKind
      else if occurences.containsAmount(3) && occurences.containsAmount(2) then HandType.FullHouse
      else if occurences.containsAmount(3) then HandType.ThreeOfAKind
      else if occurences.amountOf(2) == 2 then HandType.TwoPair
      else if occurences.amountOf(2) == 1 then HandType.OnePair
      else HighCard

    def infer(hand: Hand): HandType =
      val occurences = countOccurences(hand)
      occurences
        .get(joker)
        .map(jokersAmount =>
          if occurences == Map(joker -> 5) then FiveOfAKind
          else
            val occurencesWithoutJokers = occurences.filter((card, _) => card != joker)
            occurencesWithoutJokers
              .filter((card, _) => card != joker)
              .map((card, amount) => checkOccurences(occurencesWithoutJokers.updated(card, amount + jokersAmount)))
              .max
        )
        .getOrElse(checkOccurences(occurences))

  def parseCard(card: Char): Card = Card(card, cardRanking(card))

  def parseCards(content: String): List[Card] =
    content.map(parseCard).toList

  def parseHands(input: List[String]): List[Hand] =
    input
      .map(_.split(" "))
      .flatMap:
        case Array(cards, bid) => Some(Hand(parseCards(cards), bid.toInt))
        case _                 => None

  override def solve(input: List[String]): Int =
    val hands     = parseHands(input)
    val handTypes = hands.map(HandType.infer)
    hands
      .zip(handTypes)
      .map(Round.apply)
      .sorted
      .zipWithIndex
      .map((round, index) => round.hand.bid * (index + 1))
      .sum
