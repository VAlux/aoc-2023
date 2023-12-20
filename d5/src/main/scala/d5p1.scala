import scala.annotation.tailrec
object d5p1 extends Solution[BigInt]:

  case class Range(destination: BigInt, source: BigInt, size: BigInt):
    override def toString(): String = s"$source - $destination : $size"

  case class Mapping(val source: String, val target: String, ranges: List[Range]):
    override def toString(): String = s"$source -> $target [${ranges.mkString(" | ")}]"

  case class Almanach(val seeds: List[BigInt], val mappings: List[Mapping]):
    def seedRanges: List[List[BigInt]] = seeds.sliding(2, 2).toList
    def allSeeds: List[BigInt]         = seedRanges.flatMap(range => range.head until (range.head + range.last)).toList

    override def toString(): String =
      s"${seeds.mkString(" ")}\n${mappings.mkString("\n")}"

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

  def parseAlmanach(input: List[String]): Almanach =
    def parseSeeds(definition: String): List[BigInt] =
      definition.split(":")(1).trim().split(" ").map(BigInt(_)).toList

    def parseMappings(definition: List[String]): Option[Mapping] =
      definition.head.split("-") match
        case Array(from, "to", to) =>
          val mapping = definition.tail.flatMap { range =>
            range.split(" ").map(BigInt(_)) match
              case Array(destination, source, size) => Some(Range(destination, source, size))
              case _                                => None
          }

          Some(Mapping(from, to.replace(" map:", ""), mapping))
        case _                     => None

    val segments = segmentInput(input)
    Almanach(parseSeeds(segments.head.head), segments.tail.flatMap(parseMappings))

  def isInRange(source: BigInt, range: Range): Boolean =
    range.source <= source && source < (range.source + range.size)

  def lookup(source: BigInt, mapping: Mapping): BigInt =
    mapping.ranges
      .find(range => isInRange(source, range))
      .map(range => range.destination + (source - range.source))
      .getOrElse(source)

  def lookup(source: BigInt, mappings: List[Mapping]): BigInt =
    mappings.foldLeft(source)(lookup)

  override def solve(input: List[String]): BigInt =
    val almanach  = parseAlmanach(input)
    val locations = almanach.seeds.map(seed => lookup(seed, almanach.mappings))
    locations.min
