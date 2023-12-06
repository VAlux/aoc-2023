import d4p1.*
object d4p2 extends Solution:
  def getScoringNumbersAmount(card: Card): Int =
    extractScoringNumbers(card).size

  def copiesWon(cardId: Int, mapping: Map[Int, Int]): List[Int] =
    val copiesWonAmount = mapping(cardId)
    if copiesWonAmount == 0 then List.empty
    else ((cardId + 1) to cardId + copiesWonAmount).toList

  def totalCardsWon(cardId: Int, mapping: Map[Int, Int]): Int =
    val copies = copiesWon(cardId, mapping)
    copies.size + copies.map(copy => totalCardsWon(copy, mapping)).sum

  override def solve(input: List[String]): Int =
    val cards   = input.flatMap(parseCard)
    val grouped = cards.map(card => card.id -> getScoringNumbersAmount(card)).toMap
    cards.map(card => totalCardsWon(card.id, grouped)).sum + cards.size
