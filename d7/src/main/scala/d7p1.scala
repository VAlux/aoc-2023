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

  enum HandType(rank: Int):
    case FiveOfAKind  extends HandType(7)
    case FourOfAKind  extends HandType(6)
    case FullHouse    extends HandType(5)
    case ThreeOfAKind extends HandType(4)
    case TwoPair      extends HandType(3)
    case OnePair      extends HandType(2)
    case HighCard     extends HandType(1)

  object HandType:
    def infer(hand: Hand): HandType =
      val occurences: Map[Card, Int] = hand.cards.groupMapReduce(identity)(_ => 1)(_ + _)

  def parseHands(input: List[String]): List[Hand] =
    def parseCard(card: Char): Card = Card(card, cardRanking(card))

    def parseCards(content: String): List[Card] =
      content.map(parseCard).toList

    input.map(_.split(" ")).flatMap {
      case Array(cards, bid) => Some(Hand(parseCards(cards), bid.toInt))
      case _                 => None
    }

  override def solve(input: List[String]): Int =
    val hands = parseHands(input)
    println(hands)
    0
