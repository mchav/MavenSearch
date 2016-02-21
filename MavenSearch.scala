package chavxo.maven_search

// TODO: use scopt as an Option parser

import java.net._ 
import java.text.SimpleDateFormat
import java.time._ 
import java.util.Date
import java.util.Scanner
import scala.collection.immutable
import scala.util.matching.Regex
import scala.util.parsing.json._ 

abstract class SearchResult

case class CoordinateResult(
  groupId: String,
  artifactId: String,
  version: String,
  timestamp: LocalDateTime
) extends SearchResult

case class ClassnameResult(
  groupId: String,
  artifactId: String,
  latestVersion: String,
  timestamp: LocalDateTime,
  versionCount: Double
) extends SearchResult

case class Coordinate(
  groupId: String,
  artifactId: String,
  version: String,
  packaging: String,
  classifier: String
)

object MavenSearch {
  type PackageMap = Map[String, Any]

  val baseUrl = "http://search.maven.org/solrsearch/select?"
  val charset = java.nio.charset.StandardCharsets.UTF_8.name()

  /*
   * converts the cases of a coordinate into a search string
   */
  def showCoordinate (c : Coordinate) : String = {
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

  def constructURL (queryString : String, hits : Int, resultType: String) : String = {
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

  def timestampToDate(time: Double) : LocalDateTime = {
    val timeLong = (time).toLong
    val timeDate = new Date(timeLong)
    val timeDateStr = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(timeDate)
    LocalDateTime.parse(timeDateStr.replace(' ', 'T'))
  } 

  def parseClassResult(packageMap : PackageMap) : ClassnameResult = {
    ClassnameResult (
      packageMap("g").asInstanceOf[String], 
      packageMap("a").asInstanceOf[String], 
      packageMap("latestVersion").asInstanceOf[String], 
      timestampToDate(packageMap("timestamp").asInstanceOf[Double]), 
      packageMap("versionCount").asInstanceOf[Double]
    )  
  }

  def parseCoordinateResult(packageMap : PackageMap) : CoordinateResult = {
    CoordinateResult (
      packageMap("g").asInstanceOf[String], 
      packageMap("a").asInstanceOf[String], 
      "",//packageMap("v").asInstanceOf[String], 
      timestampToDate(packageMap("timestamp").asInstanceOf[Double])
    )  
  }

  def search(query: String) : Vector[SearchResult] = {
    val url = constructURL(query, 20, "json")
    val packageMaps = downloadPackageInfo(url)
    if (packageMaps.head.keySet.exists(_ == "v")) { 
      packageMaps.map(pkg => parseCoordinateResult(pkg)).to[Vector]
    }
    else {
      packageMaps.map(pkg => parseClassResult(pkg)).to[Vector]
    }
  }

  def downloadPackageInfo(searchUrl : String) : List[PackageMap] = {
    val myURL = new URL(searchUrl)
    val myURLConnection = myURL.openConnection()
    myURLConnection.connect()
    val stream = myURLConnection.getInputStream()

    // read input stream as string
    val s = new java.util.Scanner(stream).useDelimiter("\\A");
    val res = s.next()

    val packages = JSON.parseFull(res)

    packages.get.asInstanceOf[PackageMap]("response").asInstanceOf[Map[String, List[PackageMap]]]("docs")
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
    val result = search(searchTerm)

    result.map (x => println(x))
  }
}