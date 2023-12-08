import scala.annotation.tailrec
object d5p1 extends Solution:

  case class Mapping(val source: String, val target: String, mapping: Map[Long, Long])
  case class Almanach(val seeds: List[Long], val mappings: List[Mapping])

  def segmentInput(input: List[String]): List[List[String]] =
    @tailrec
    def go(current: String, rem: List[String], acc: List[List[String]] = List.empty): List[List[String]] =
      if rem.isEmpty then acc
      else
        current match
          case ""   => go(rem.head, rem.tail, acc :+ List())
          case line => go(rem.head, rem.tail, acc.updated(acc.size - 1, acc.last :+ current))

    input match
      case first :: rest => go(rest.head, rest.tail, List(List(first)))
      case _             => List.empty

  def parseSeeds(input: String): List[Long] =
    ???

  def parseMappings(input: List[List[String]]): List[Mapping] =
    def parseMapping(mapping: List[String]): Option[Mapping] =
      mapping match
        case title :: numbers => ???
        case _                => None

    input.flatMap(parseMapping)

  def parseAlmanach(input: List[String]): Option[Almanach] =
    val segments = segmentInput(input)
    segments.foreach(println)
    segments match
      case List(seedsContent) :: mappingsContent =>
        val seeds    = parseSeeds(seedsContent)
        val mappings = parseMappings(mappingsContent)
        Some(Almanach(seeds, mappings))
      case _                                     => None

  override def solve(input: List[String]): Int =
    val almanach = parseAlmanach(input)
    0
