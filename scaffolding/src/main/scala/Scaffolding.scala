import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.Console.*
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.model.Node
import net.ruippeixotog.scalascraper.model.ElementNode
import net.ruippeixotog.scalascraper.model.TextNode

object Scaffolding:
  @main def entrypoint(day: Int) =
    // val article   = getDayTask(2)
    // val processed = article.map(processElement).mkString
    // println(article.map(_.innerHtml).mkString)
    // println(processed)
    generateScaffoldingForDay(day)

  def generateScaffoldingForDay(day: Int) =
    val root         = createDirectory(Paths.get("."), s"d$day")
    val scalaSources = createDirectory(root, "src", "main", "scala")
    val resources    = createDirectory(root, "src", "main", "resources")

    val readme      = createFile(root, "README.md")
    val testInputP1 = createFile(resources, "input-test-p1.txt")
    val testInputP2 = createFile(resources, "input-test-p2.txt")
    val mainInput   = createFile(resources, "input.txt")

    val mainSource = createFile(scalaSources, s"d$day.scala")
    val sourceP1   = createFile(scalaSources, s"d${day}p1.scala")
    val sourceP2   = createFile(scalaSources, s"d${day}p2.scala")

    val replacementRules: Map[String, String] = Map(
      "day" -> day.toString()
    )

    val mainSourceContent = FileLoader.readFile("MainSourceTemplate.scala")
    val p1SourceContent   = FileLoader.readFile("p1SourceTemplate.scala")
    val p2SourceContent   = FileLoader.readFile("p2SourceTemplate.scala")

    writeScaffoldingCode(processTemplate(mainSourceContent, replacementRules), mainSource)
    writeScaffoldingCode(processTemplate(p1SourceContent, replacementRules), sourceP1)
    writeScaffoldingCode(processTemplate(p2SourceContent, replacementRules), sourceP2)

    println(s"$GREEN scaffolding generation for day $day DONE!")

  def processTemplate(input: String, replacementRules: Map[String, String]): String =
    replacementRules.foldLeft(input) { case (acc, (from, to)) => acc.replaceAll(s"__${from}__", to) }

  def writeScaffoldingCode(content: String, file: Path) =
    Files.write(file, content.getBytes())

  def getDayTask(day: Int): List[Element] =
    import net.ruippeixotog.scalascraper.dsl.DSL._
    import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
    import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

    val browser  = JsoupBrowser()
    val document = browser.get(s"https://adventofcode.com/2023/day/$day")
    document >> elementList("code")

  def processElement(root: Element): String =
    def processTag(tag: Node): String =
      tag match
        case ElementNode(element) =>
          element.tagName match
            case "h1"   => s"# ${process(element.childNodes.toList).mkString("\n\n")}"
            case "h2"   => s"## ${process(element.childNodes.toList).mkString("\n\n")}"
            case "h3"   => s"### ${process(element.childNodes.toList).mkString("\n\n")}"
            case "code" => s"```\n${process(element.childNodes.toList).mkString("\n\n")}\n```"
            case _      => process(element.childNodes.toList).mkString("\n\n")
        case TextNode(content)    => content

    def process(childNodes: List[Node], acc: List[String] = List.empty): List[String] =
      if childNodes.isEmpty then acc
      else
        childNodes match
          case tag :: rest => process(rest, acc :+ processTag(tag))
          case _           => acc

    processTag(ElementNode(root)).mkString

  def createDirectory(root: Path, path: String*): Path =
    println(s"$YELLOW directory at $root for path: ${path.mkString("/")}")
    val pathSegments = path.toList
    Files.createDirectories(root.resolve(Paths.get(pathSegments.head, pathSegments.tail*)))

  def createFile(root: Path, filename: String): Path =
    println(s"$YELLOW file at $root with name: $filename")
    Files.createFile(root.resolve(filename))
