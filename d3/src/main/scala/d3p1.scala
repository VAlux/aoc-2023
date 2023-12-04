import d3p1.SchematicsEntry.*
import scala.annotation.tailrec
object d3p1 extends Solution:

  enum SchematicsEntryType:
    case DOT
    case DIGIT
    case SYMBOL

  object SchematicsEntryType:
    def infer(input: Char): SchematicsEntryType =
      input match
        case '.'              => DOT
        case ch if ch.isDigit => DIGIT
        case _                => SYMBOL

  case class Location(val row: Int, val column: Int)

  sealed trait SchematicsEntry(val location: Location):
    val length: Int

  object SchematicsEntry:
    case class PartNumber(val value: Int, override val location: Location) extends SchematicsEntry(location):
      override val length: Int = value.toString().length()

    case class PartSymbol(val symbol: Char, override val location: Location) extends SchematicsEntry(location):
      override val length: Int = 1

  def parseEngineSchematics(input: List[String]): List[SchematicsEntry] =
    import SchematicsEntryType.*

    def appendContext(
      context: List[Char],
      mode: SchematicsEntryType,
      acc: List[SchematicsEntry],
      location: Location
    ): List[SchematicsEntry] =
      if context.isEmpty then acc
      else
        mode match
          case DOT    => acc
          case DIGIT  => acc :+ PartNumber(context.mkString.toInt, location)
          case SYMBOL => acc :+ PartSymbol(context.head, location)

    @tailrec
    def parse(
      current: (Char, Int),
      rem: List[(Char, Int)],
      currentMode: SchematicsEntryType,
      rowIndex: Int,
      context: List[Char] = List.empty,
      acc: List[SchematicsEntry] = List.empty
    ): List[SchematicsEntry] =
      if rem.isEmpty then acc
      else
        current match
          case ('.', index) =>
            if currentMode == DOT
            then parse(rem.head, rem.tail, DOT, rowIndex, context, acc)
            else
              val newState = appendContext(context, currentMode, acc, Location(rowIndex, index - context.length))
              parse(rem.head, rem.tail, DOT, rowIndex, List.empty, newState)
          case (ch, index)  =>
            val newMode = SchematicsEntryType.infer(ch)
            if newMode == currentMode then parse(rem.head, rem.tail, currentMode, rowIndex, context :+ ch, acc)
            else
              val newState = appendContext(context, currentMode, acc, Location(rowIndex, index - context.length))
              parse(rem.head, rem.tail, newMode, rowIndex, List(ch), newState)

    input.zipWithIndex
      .map((row, index) => (row.toCharArray().toList.zipWithIndex, index))
      .flatMap((row, index) => parse(row.head, row.tail, SchematicsEntryType.infer(row(0)._1), index))

  override def solve(input: List[String]): Int =
    println(parseEngineSchematics(input).mkString("\n"))
    0
