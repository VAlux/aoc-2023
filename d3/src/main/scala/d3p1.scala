import d3p1.SchematicsEntry.*
import scala.annotation.tailrec
import scala.runtime.stdLibPatches.language.deprecated.symbolLiterals
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

  sealed trait SchematicsEntry[T]:
    val location: Location
    val length: Int
    val value: T

    def bodyLocation: Set[Location] =
      val row    = location.row
      val column = location.column
      (0 to length).map(index => Location(row, column + index)).toSet

    def adjacentLocations: Set[Location] =
      val row       = location.row
      val column    = location.column
      val topRow    = (0 to length + 1).map(index => Location(row - 1, column + index - 1)).toSet
      val bottomRow = (0 to length + 1).map(index => Location(row + 1, column + index - 1)).toSet
      val leftEdge  = Location(row, column - 1)
      val rightEdge = Location(row, column + length)
      topRow ++ bottomRow ++ List(leftEdge, rightEdge)

  object SchematicsEntry:
    case class PartNumber(val partNumber: Int, override val location: Location) extends SchematicsEntry[Int]:
      override val length: Int = partNumber.toString().length()
      override val value: Int  = partNumber

    case class PartSymbol(val symbol: Char, override val location: Location) extends SchematicsEntry[Char]:
      override val length: Int = 1
      override val value: Char = symbol

  def parseEngineSchematics(input: List[String]): List[SchematicsEntry[_]] =
    import SchematicsEntryType.*

    def appendContext(
      context: List[Char],
      mode: SchematicsEntryType,
      acc: List[SchematicsEntry[_]],
      location: Location
    ): List[SchematicsEntry[_]] =
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
      acc: List[SchematicsEntry[_]] = List.empty
    ): List[SchematicsEntry[_]] =
      if rem.isEmpty then appendContext(context, currentMode, acc, Location(rowIndex, current._2 - context.length))
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

  end parseEngineSchematics

  def locateParts(schematics: List[SchematicsEntry[_]]): List[PartNumber] =
    val (symbols, parts) = schematics.partition(_.isInstanceOf[PartSymbol])
    symbols
      .map(_.adjacentLocations)
      .flatMap(locations => parts.filter(part => locations.intersect(part.bodyLocation).nonEmpty))
      .distinct
      .map(_.asInstanceOf[PartNumber])

  override def solve(input: List[String]): Int =
    val parts = locateParts(parseEngineSchematics(input))
    parts.map(_.value).sum
