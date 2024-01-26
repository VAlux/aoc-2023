import d8p1.*
import d8p1.Network.NodeLink
import d8p1.Network.NetworkGraph
object d8p2 extends Solution[Int]:
  def getAllStartNodes(network: NetworkGraph): NetworkGraph =
    network.filter((node, _) => node.value.endsWith("A"))

  override def solve(input: List[String]): Int =
    val startNodes = for
      map       <- MapDefinition.parse(input)
      startNodes = getAllStartNodes(map.network)
    yield startNodes

    println(startNodes)
    0
