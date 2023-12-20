import scala.annotation.tailrec
import d1p1.*

object d1p2 extends Solution[Int]:
  val numbersMapping: List[(String, String)] =
    List(
      "one"   -> "1",
      "two"   -> "2",
      "three" -> "3",
      "four"  -> "4",
      "five"  -> "5",
      "six"   -> "6",
      "seven" -> "7",
      "eight" -> "8",
      "nine"  -> "9"
    ).sortBy((word, _) => -word.length())

  override def solve(input: List[String]): Int =
    input.map(replaceWordsWithNumbers).map(extractDigitsList).map(extractNumber).sum

  def replaceWordsWithNumbers(line: String): String =
    @tailrec
    def replace(current: String, acc: String = ""): String =
      if current.isBlank() then acc
      else
        numbersMapping.find((word, _) => current.startsWith(word)) match
          case None              => replace(current.tail, acc + current.head)
          case Some((_, number)) => replace(current.tail, acc + number)

    replace(line)
