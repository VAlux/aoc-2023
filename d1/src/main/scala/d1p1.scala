object d1p1 extends Solution:
  override def solve(input: List[String]): Int =
    input.map(extractDigitsList).map(extractNumber).sum

  def extractDigitsList(input: String): List[Char] =
    input.filter(_.isDigit).toList

  def extractNumber(input: List[Char]): Int =
    s"${input.head}${input.last}".toInt
