import Extensions.segment
import scala.annotation.tailrec
import d8p1.Network.NetworkGraph
object d8p1 extends Solution[Int]:

  enum StepType:
    case Left
    case Right

  object StepType:
    def infer(source: Char): Option[StepType] =
      source match
        case 'L' => Some(StepType.Left)
        case 'R' => Some(StepType.Right)
        case _   => None

  case class Command(steps: List[StepType]):
    override def toString(): String =
      steps.map {
        case StepType.Left  => "<-"
        case StepType.Right => "->"
      }.mkString(" ")

    def getStep(index: Int): StepType =
      steps((index + steps.size) % steps.size)

  object Command:
    def parse(input: String): Command =
      Command(input.toCharArray().flatMap(StepType.infer).toList)

  object Network:
    type NetworkGraph = Map[Node, NodeLink]

    case class Node(value: String):
      override def toString(): String = value

    case class NodeLink(left: Node, right: Node)

    def parse(input: List[String]): NetworkGraph =
      def parseLink(link: String): Option[NodeLink] =
        link.filter(ch => ch != '(' && ch != ')').split(", ") match
          case Array(left, right) => Some(NodeLink(Node(left), Node(right)))
          case _                  => None

      def parseMapping(mapping: String): Option[(Node, NodeLink)] =
        mapping.split(" = ") match
          case Array(nodeValue, linkValue) =>
            for
              link <- parseLink(linkValue)
              node  = Node(nodeValue)
            yield node -> link
          case _                           => None

      input.flatMap(parseMapping).toMap

  case class MapDefinition(command: Command, network: NetworkGraph):
    import Network.Node

    override def toString(): String =
      s"$command\n${network.map((node, link) => s"$node = (${link.left}, ${link.right})").mkString("\n")}"

    def step(step: StepType, node: Node): Node =
      step match
        case StepType.Left  => network(node).left
        case StepType.Right => network(node).right

  object MapDefinition:
    import Network.*

    def parse(input: List[String]): Option[MapDefinition] =
      input match
        case command :: networkNodes => Some(MapDefinition(Command.parse(command), Network.parse(networkNodes)))
        case _                       => None

    def walk(map: MapDefinition, start: Node, end: Node): List[Node] =
      @tailrec
      def go(current: StepType, node: Node, index: Int = 0, acc: List[Node] = List.empty): List[Node] =
        if acc.lastOption.exists(_ == end) then acc
        else
          val newNode   = map.step(current, node)
          val nextIndex = index + 1
          go(map.command.getStep(nextIndex), newNode, nextIndex, acc :+ newNode)

      go(map.command.getStep(0), start)

  override def solve(input: List[String]): Int =
    MapDefinition
      .parse(input)
      .map(map => MapDefinition.walk(map, Network.Node("AAA"), Network.Node("ZZZ")))
      .getOrElse(List.empty)
      .size
