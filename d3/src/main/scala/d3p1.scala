import d3p1.SchematicsEntry.*
import scala.annotation.tailrec
import scala.runtime.stdLibPatches.language.deprecated.symbolLiterals
import Extensions.*

object d3p1 extends Solution[Int]:

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

  case class Location(val row: Int, val column: Int):
    override def toString(): String = s"[$row, $column]"

  sealed trait SchematicsEntry[T]:
    val location: Location
    val length: Int
    val value: T

    def bodyLocation: Set[Location] =
      val row    = location.row
      val column = location.column
      (0 until length).map(index => Location(row, column + index)).toSet

    def adjacentLocations: Set[Location] =
      val row    = location.row
      val column = location.column
      val top    = (0 to length + 1).map(index => Location(row - 1, column + index - 1)).toSet
      val bottom = (0 to length + 1).map(index => Location(row + 1, column + index - 1)).toSet
      val left   = Location(row, column - 1)
      val right  = Location(row, column + length)
      top ++ bottom ++ List(left, right)

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
      mode: SchematicsEntryType,
      rowIndex: Int,
      context: List[Char] = List.empty,
      acc: List[SchematicsEntry[_]] = List.empty
    ): List[SchematicsEntry[_]] =
      if rem.isEmpty then
        current match
          case ('.', index) => appendContext(context, mode, acc, Location(rowIndex, index - context.length))
          case (ch, index)  => appendContext(context :+ ch, mode, acc, Location(rowIndex, index - context.length))
      else
        current match
          case ('.', index) =>
            if mode == DOT
            then parse(rem.head, rem.tail, DOT, rowIndex, context, acc)
            else
              val newState = appendContext(context, mode, acc, Location(rowIndex, index - context.length))
              parse(rem.head, rem.tail, DOT, rowIndex, List.empty, newState)
          case (ch, index)  =>
            val newMode = SchematicsEntryType.infer(ch)
            if newMode == mode then parse(rem.head, rem.tail, mode, rowIndex, context :+ ch, acc)
            else
              val newState = appendContext(context, mode, acc, Location(rowIndex, index - context.length))
              parse(rem.head, rem.tail, newMode, rowIndex, List(ch), newState)

    input
      .map(_ + ".") // append dot to each line to simplify recursion exit case during parsing
      .zipWithIndex
      .map((row, index) => (row.toCharArray().toList.zipWithIndex, index))
      .flatMap((row, index) => parse(row.head, row.tail, SchematicsEntryType.infer(row(0)._1), index))

  end parseEngineSchematics

  def locateParts(schematics: List[SchematicsEntry[_]]): List[PartNumber] =
    val (symbols, parts) = schematics.partition(_.isInstanceOf[PartSymbol])
    val symbolLocations  = symbols.groupBy(_.location)
    parts
      .filter(part => part.adjacentLocations.exists(location => symbolLocations.contains(location)))
      .distinct
      .map(_.asInstanceOf[PartNumber])

  override def solve(input: List[String]): Int =
    locateParts(parseEngineSchematics(input)).map(_.value).sum
