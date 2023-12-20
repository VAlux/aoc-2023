import scala.annotation.tailrec
import d5p1.Mapping
import java.math.BigInteger
object d5p2 extends Solution[BigInt]:

  val positiveInfinity = BigInt(-1)

  extension (current: BigInt)
    def min2(other: BigInt): BigInt =
      if other == positiveInfinity then current
      else if current == positiveInfinity then other
      else current.min(other)

  @tailrec
  def lookup(seed: BigInt, total: BigInt, mappings: List[Mapping], location: BigInt = positiveInfinity): BigInt =
    if seed >= total then location
    else
      val current = d5p1.lookup(seed, mappings)
      lookup(seed + 1, total, mappings, location.min2(current))

  @tailrec
  def lookup(
    range: List[BigInt],
    remRanges: List[List[BigInt]],
    mappings: List[Mapping],
    location: BigInt
  ): BigInt =
    val newLocation = lookup(range.head, range.head + range.last, mappings)
    if remRanges.isEmpty then location.min2(newLocation)
    else
      val minLocation = location.min2(newLocation)
      lookup(remRanges.head, remRanges.tail, mappings, minLocation)

  override def solve(input: List[String]): BigInt =
    val almanach = d5p1.parseAlmanach(input)
    val ranges   = almanach.seedRanges
    d5p2.lookup(ranges.head, ranges.tail, almanach.mappings, positiveInfinity).intValue
