object d7p1 extends Solution[Int]:

  val cardRanking = Map(
    'A' -> 13,
    'K' -> 12,
    'Q' -> 11,
    'J' -> 10,
    'T' -> 9,
    '9' -> 8,
    '8' -> 7,
    '7' -> 6,
    '6' -> 5,
    '5' -> 4,
    '4' -> 3,
    '3' -> 2,
    '2' -> 1
  )

  case class Card(name: Char, rank: Int)
  case class Hand(cards: List[Card], bid: Int)
  case class Round(hand: Hand, handType: HandType)

  enum HandType(val rank: Int):
    case FiveOfAKind  extends HandType(7)
    case FourOfAKind  extends HandType(6)
    case FullHouse    extends HandType(5)
    case ThreeOfAKind extends HandType(4)
    case TwoPair      extends HandType(3)
    case OnePair      extends HandType(2)
    case HighCard     extends HandType(1)

  object HandType:
    def infer(hand: Hand): HandType =
      extension (a: Map[Card, Int])
        def containsAmount(n: Int): Boolean = a.values.exists(_ == n)
        def amountOf(n: Int): Int           = a.values.count(_ == n)

      val occurences: Map[Card, Int] = hand.cards.groupMapReduce(identity)(_ => 1)(_ + _)

      if occurences.containsAmount(5) then HandType.FiveOfAKind
      else if occurences.containsAmount(4) then HandType.FourOfAKind
      else if occurences.containsAmount(3) && occurences.containsAmount(2) then HandType.FullHouse
      else if occurences.containsAmount(3) then HandType.ThreeOfAKind
      else if occurences.amountOf(2) == 2 then HandType.TwoPair
      else if occurences.amountOf(2) == 1 then HandType.OnePair
      else HighCard

  given roundOrdering: Ordering[Round] with
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

  def parseHands(input: List[String]): List[Hand] =
    def parseCard(card: Char): Card = Card(card, cardRanking(card))

    def parseCards(content: String): List[Card] =
      content.map(parseCard).toList

    input.map(_.split(" ")).flatMap {
      case Array(cards, bid) => Some(Hand(parseCards(cards), bid.toInt))
      case _                 => None
    }

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
