import d3p1.*
import d3p1.SchematicsEntry.PartNumber
import d3p1.SchematicsEntry.PartSymbol
import Extensions.*
object d3p2 extends Solution:

  case class Gear(ratioComponents: List[Int]):
    val ratio = ratioComponents.reduce(_ * _)

  def locateGearSymbols(schematics: List[SchematicsEntry[_]]): List[PartSymbol] =
    schematics.filter(entry => entry.value == "*").map(_.asInstanceOf[PartSymbol])

  def locateGears(schematics: List[SchematicsEntry[_]]): List[Gear] =
    val (symbols, parts) = schematics.partition(_.isInstanceOf[PartSymbol])
    symbols.filter(_.value == '*').flatMap { symbol =>
      val adjacentParts = parts
        .filter(part => symbol.adjacentLocations.containsAny(part.bodyLocation))
        .map(_.asInstanceOf[PartNumber])

      if adjacentParts.size == 2 then Some(Gear(adjacentParts.map(_.value))) else None
    }

  override def solve(input: List[String]): Int =
    val schematics = parseEngineSchematics(input)
    val gears      = locateGears(schematics)
    gears.map(_.ratio).sum
