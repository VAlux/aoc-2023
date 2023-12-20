import scala.annotation.tailrec
import d5p1.Mapping
import java.math.BigInteger
object d5p2 extends Solution[BigInt]:

  // emulation
  val positiveInfinity = BigInt(-1)

  extension (current: BigInt)
    // version of BigInt min, including positive infinity emulation,
    // this is a bad decision in general and applies only to the provided input data
    def minWithPosInf(other: BigInt): BigInt =
      if other == positiveInfinity then current
      else if current == positiveInfinity then other
      else current.min(other)

  @tailrec
  def lookup(seed: BigInt, total: BigInt, mappings: List[Mapping], location: BigInt = positiveInfinity): BigInt =
    if seed >= total then location
    else
      val current = d5p1.lookup(seed, mappings)
      lookup(seed + 1, total, mappings, location.minWithPosInf(current))

  @tailrec
  def lookup(
    range: List[BigInt],
    remRanges: List[List[BigInt]],
    mappings: List[Mapping],
    location: BigInt
  ): BigInt =
    val newLocation = lookup(range.head, range.head + range.last, mappings)
    if remRanges.isEmpty then location.minWithPosInf(newLocation)
    else
      val minLocation = location.minWithPosInf(newLocation)
      lookup(remRanges.head, remRanges.tail, mappings, minLocation)

  override def solve(input: List[String]): BigInt =
    val almanach = d5p1.parseAlmanach(input)
    val ranges   = almanach.seedRanges
    d5p2.lookup(ranges.head, ranges.tail, almanach.mappings, positiveInfinity).intValue
