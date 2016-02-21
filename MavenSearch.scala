package chavxo.maven_search

// TODO: use scopt as an Option parser

import java.net._ 
import java.text.SimpleDateFormat
import java.time._ 
import java.util.Date
import scala.util.parsing.json._ 

case class CoordinateResult(
  groupId: String,
  artifactId: String,
  version: String,
  timestamp: LocalDateTime
)

case class ClassnameResult(
  groupId: String,
  artifactId: String,
  latestVersion: String,
  timestamp: LocalDateTime,
  versionCount: Double
)

case class Coordinate(
  groupId: String,
  artifactId: String,
  version: String,
  packaging: String,
  classifier: String
)

object MavenSearch {
  private val baseUrl = "http://search.maven.org/solrsearch/select?"
  private val charset = java.nio.charset.StandardCharsets.UTF_8.name()

  /*
   * converts the cases of a coordinate into a search string
   */
  private def showCoordinate (c : Coordinate) : String = {
    val group = if (c.groupId != "") {
    String.format("g:\"%s\" ", c.groupId)
    } else ""

    val artifact = if (c.artifactId != "") {
    String.format("a:\"%s\" ", c.artifactId)
    } else ""

    val version = if (c.version != "") {
    String.format("v:\"%s\" ", c.version)
    } else ""

    return String.format("%s%s%s", group, artifact, version)
  }

  private def constructURL (queryString : String, hits : Int, resultType: String) : String = {
    val rows = String.valueOf(hits)
    val wt = resultType

    val complete = String.format(baseUrl + "q=%s&rows=%s&wt=%s", 
      URLEncoder.encode(queryString, charset), 
      URLEncoder.encode(rows, charset),
      URLEncoder.encode(wt, charset)
    )
    // println(complete)
    complete
  }

  private def timestampToDate(time: Double) : LocalDateTime = {
    val timeLong = (time).toLong
    val timeDate = new Date(timeLong)
    val timeDateStr = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(timeDate)
    LocalDateTime.parse(timeDateStr.replace(' ', 'T'))
  } 


  def byCoordinate(query : String): Vector[CoordinateResult] = {
    val url = constructURL(query, 20, "json")
    downloadPackageInfo(
      url,
      packageMap =>
        CoordinateResult (
          packageMap("g").asInstanceOf[String], 
          packageMap("a").asInstanceOf[String], 
          packageMap("v").asInstanceOf[String], 
          timestampToDate(packageMap("timestamp").asInstanceOf[Double])
        )
    )
  }

  private def parseClassResult(packageMap : Map[String, Any]) : ClassnameResult = {
    ClassnameResult (
      packageMap("g").asInstanceOf[String], 
      packageMap("a").asInstanceOf[String], 
      packageMap("latestVersion").asInstanceOf[String], 
      timestampToDate(packageMap("timestamp").asInstanceOf[Double]), 
      packageMap("versionCount").asInstanceOf[Double]
    ) 
  }

  def byClassname(query : String): Vector[ClassnameResult] = {
    val url = constructURL(query, 20, "json")
    downloadPackageInfo(url, parseClassResult)
  }

  def basic(query: String) : Vector[ClassnameResult] = {
    val url = constructURL(query, 20, "json")
    downloadPackageInfo(url, parseClassResult)
  }
 
  private def red( s: String ) = Console.RED + s + Console.RESET
  private def green( s: String ) = Console.GREEN + s + Console.RESET
  private def blue( s: String ) = Console.BLUE + s + Console.RESET

  private def prettyPrint(json: Any, indent: Int = 0): String = {
    val space = ("  "*indent)
    val string: String = json match {
      case m:Map[_,_] =>
        lazy val align = " " * m.keys.map(_.toString.size).max
        val elements =  m.map{
          case (key, value) => "\n" + space + "  " + key + ": " + align.take(align.size - key.toString. size) + prettyPrint(value, indent+1)
        }.mkString(",")
        "{" + (if(elements.size > 0) elements +"\n" + space else "") + "}"
      case m:List[_] => 
        val elements = m.map{
          case value => "\n" + space + "  " + prettyPrint(value, indent+1)
        }.mkString(",")
        "[" + (if(elements.size > 0) elements +"\n" + space else "")  + "]"
      case v: String => red("\"" + v + "\"")
      case v: Int => blue(v.toString)
      case v: Float => green(v.toString)
      case v: Double => green(v.toString)
      case v => v.toString
    }
    val inline = string.replace("\n", " ").replaceAll(" +"," ")
    if(inline.replaceAll("[\\p{C}]","").size < 140) inline else string
  }

  def downloadPackageInfo[T](searchUrl : String, typeResult: Map[String, Any] => T) : Vector[T] = {
    val myURL = new URL(searchUrl)
    val myURLConnection = myURL.openConnection()
    myURLConnection.connect()
    val stream = myURLConnection.getInputStream()

    // read input stream as string
    val s = new java.util.Scanner(stream).useDelimiter("\\A");
    val res = s.next()

    val json = JSON.parseFull(res).getOrElse( throw new Exception( "Could not parse json: " + res ) )

    try{
      json.asInstanceOf[
        Map[String,
          Map[String,
            List[Map[String, Any]
      ]]]]("response")("docs").map( entry =>
        try{
          typeResult(entry)
        } catch {
          case e:java.util.NoSuchElementException => throw new Exception( "Could not parse entry:\n" + prettyPrint(entry), e )
        }
      ).to[Vector]
      
    } catch{
      case e:java.util.NoSuchElementException => throw new Exception( "Could not parse results:\n" + prettyPrint(json), e )
    }
  }

  def main(args : Array[String]) : Unit = {
    if (args.length == 0 || args(0) == "--help") {
      println("Maven Search tool 0.0.1\n\tUsage: scala MavenSearch [package name]")
      println("\t       scala MavenSearch [option] [argument]")
      println("\nOptions:")
      println("\t-g \tsearch for a group\n\t-fc \tsearch for a classname\n")    
      return
    }

    def getOption(option : String) : String = {
      val index = args.indexOf(option)
      if (index != (-1)) 
        return args(index + 1) 
      return ""
    }

    val fc = getOption("-fc")
    val g  = getOption("-g")
    val a  = getOption("-a")
    val id = getOption("-id")
    val v  = getOption("-v")
    val p  = getOption("-p")
    val l  = getOption("-l")
    val c  = getOption("-c")

    val searchTerm = if (fc != "") {
        String.format("fc:\"%s\"", c)
      } else if (c != "") {
        String.format("c:\"%s\"", c)
      } else if (g != "" || id != "" || a != "") {
        showCoordinate(Coordinate (g, a, v, p, c)).split(" ").toList.mkString(" AND ")
      } else {
        args(0)
      }
    
    val result = if (v != "" || fc != "") {
      byCoordinate(searchTerm)
    } else {
      basic(searchTerm)
    }

    result.map (x => println(x))
  }
}
