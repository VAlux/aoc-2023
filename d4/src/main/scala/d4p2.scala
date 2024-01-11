import d4p1.*
object d4p2 extends Solution[Int]:
  def getScoringNumbersAmount(card: Card): Int =
    extractScoringNumbers(card).size

  def copiesWon(cardId: Int, mapping: Map[Int, Int]): List[Int] =
    ((cardId + 1) to cardId + mapping(cardId)).toList

  def totalCardsWon(cardId: Int, mapping: Map[Int, Int]): Int =
    val copies = copiesWon(cardId, mapping)
    copies.size + copies.map(copy => totalCardsWon(copy, mapping)).sum

  override def solve(input: List[String]): Int =
    val cards        = input.flatMap(parseCard)
    val scoreMapping = cards.map(card => card.id -> getScoringNumbersAmount(card)).toMap
    cards.map(card => totalCardsWon(card.id, scoreMapping)).sum + cards.size
