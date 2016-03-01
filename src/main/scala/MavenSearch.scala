// TODO: Optional search with OR

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

case class ClassVector(value: Vector[ClassnameResult])
case class CoordVector(value: Vector[CoordinateResult])

case class Coordinate(
  groupId: String,
  artifacts: List[String], // allows for the artifacts to have different scala/hava versions
  version: String,
  packaging: String,
  classifier: String
)

object MavenSearch {
  private val baseUrl = "http://search.maven.org/solrsearch/select?"
  private val charset = java.nio.charset.StandardCharsets.UTF_8.name()
  private val options = Array("--fully-qualified", "--class", 
    "--group", "--artifact", "--version-number", "--packaging", "--classifier",
    "--force-versions", "--scala-version")

  /*
   * converts the cases of a coordinate into a search string
   * e.g Coordinate(org.chav, chav) -> g:"org.chav"+AND+a:"chav"
   */
  private def showCoordinate (c : Coordinate) : String = {
    val group = if (c.groupId != "") {
    String.format("g:\"%s\" ", c.groupId)
    } else ""

    val artifact = if (!c.artifacts.isEmpty) {
    	c.artifacts.map(x => String.format("a:\"%s\"", x)).mkString("+OR+")
    } else ""

    val version = if (c.version != "") {
    String.format("v:\"%s\" ", c.version)
    } else ""

    String.format("%s%s%s", group, artifact, version)
  }

  /*
   * constructs the search URL by attaching the options to the base url
   */
  private def constructURL (queryString : String, hits : Int, resultType: String) : String = {
    println(queryString)
    val rows = String.valueOf(hits)
    val wt = resultType

    val complete = String.format(baseUrl + "q=%s&rows=%s&wt=%s", 
      queryString.split('+').map(URLEncoder.encode(_, charset)).mkString("+"), // accommodate compounded OR in artfact 
      URLEncoder.encode(rows, charset),
      URLEncoder.encode(wt, charset)
    )
    complete
  }

  /*
   * convert timestamp to localdate
   */
  private def timestampToDate(time: Double) : LocalDateTime = {
    val timeLong = (time).toLong
    val timeDate = new Date(timeLong)
    val timeDateStr = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(timeDate)
    LocalDateTime.parse(timeDateStr.replace(' ', 'T'))
  } 


  /*
   * performs the equivalent of a maven advanced coordinate search and returns Coordinate result
   */
  def byCoordinate(query : String, forceVersions : Boolean): CoordVector = {
    val url = constructURL(query, 20, "json") + (if (forceVersions) "&core=gav" else "")
    val res = downloadPackageInfo(
      url,
      packageMap => // package info function
        CoordinateResult (
          packageMap("g").asInstanceOf[String], 
          packageMap("a").asInstanceOf[String], 
          packageMap("v").asInstanceOf[String], 
          timestampToDate(packageMap("timestamp").asInstanceOf[Double])
        )
    )
    CoordVector(res)
  }

  /*
   * parse result from JSON to class result
   */
  private def parseClassResult(packageMap : Map[String, Any]) : ClassnameResult = {
    ClassnameResult (
      packageMap("g").asInstanceOf[String], 
      packageMap("a").asInstanceOf[String], 
      packageMap("latestVersion").asInstanceOf[String], 
      timestampToDate(packageMap("timestamp").asInstanceOf[Double]), 
      packageMap("versionCount").asInstanceOf[Double]
    ) 
  }


  /*
   * perfoms the equivalent of a maven advanced search by classname
   */
  def byClassname(query : String): ClassVector = {
    val url = constructURL(query, 20, "json")
    val res = downloadPackageInfo(url, parseClassResult)
    ClassVector(res)
  }

  /*
   * equivalent to typing query into basic search box
   */
  def basic(query: String) : ClassVector = {
    val url = constructURL(query, 20, "json")
    val res = downloadPackageInfo(url, parseClassResult)
    ClassVector(res)
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

  private def printOptions() : Unit = {
      println("""
Maven Search tool
  scala MavenSearch [option] [argument] [flag]

  Options:
  
  --fully-qualified : search by fully qualified package name
  
  --class : search by class name
  
  --group : search by groupId
  
  --artifact : search by artifactID
  
  --version-number : search by version number (used in conjunction with other options)
  
  --packaging : search by packaging (*.jar or *.pom)
  
  --classifier : search by classifier
  
  --scala-version : include scala version of artifact. Default is 2.11

  Flags:

  --force-versions : coerces the output to coordinate type
     """.trim+"\n")
  }

  /*
   * return a string with the results in a format usable with sbt
   */
  def showClassResults(results : Vector[ClassnameResult]) : String = {
    var res = ""
    val grouped = results.groupBy(x => x.groupId) 
    for (key <- grouped.keySet) {
      res += key + "\n"
      val regrouped = grouped(key).groupBy(x => x.artifactId)
      for (pkg <- regrouped.keySet) {
        res += "  " + pkg + "\n"
        res += "    latest version: " + regrouped(pkg).map(x => x.latestVersion).mkString(", ") + "\n"
      }

    }
    
    return res
  }

  private def semanticVersionLessThan(left: String, right: String) = {
    def toInt(str: String): Either[Int,String] = try {
      Left(str.toInt)
    } catch {
      case e: NumberFormatException => Right(str)
    }

    // FIXME: this ignores ends when different size
    val zipped = left.split("\\.|\\-").map(toInt) zip right.split("\\.|\\-").map(toInt)
    val res = zipped.map {
      case (Left(i),Left(j)) => i compare j
      case (Right(i),Right(j)) => i compare j
      case (Left(i),Right(j)) => i.toString compare j
      case (Right(i),Left(j)) => i compare j.toString
    }
    res.find(_ != 0).map(_ < 0).getOrElse(false)
  }

  private def stableVersion(version: String) = version.replaceAll("[0-9\\.]*","") == ""

  /*
   * return a string with the results in a format usable with sbt
   */
  def showCoordResults(results : Vector[CoordinateResult]) : String = {
      results
        .groupBy( x => (x.groupId, x.artifactId) )
        .mapValues(
          _.sortBy(_.version)( Ordering.fromLessThan(semanticVersionLessThan) )
           .reverse
        )
        .toList
        .sortBy(_._1)
        .map{ case ( (groupId, artifactId), results ) =>
          val versions = results.map(_.version)
          val stable = versions.filter(stableVersion).headOption
          val sbtStable = stable.map{ v => s"""\"$groupId\" %% ${artifactId.takeWhile(_ != '_')} % \"$v\"""" }
          val otherVersions = versions.filterNot(Some(_) == stable).mkString(", ")
          val others = if (otherVersions.isEmpty) "-" else otherVersions
          val date = "\\\\"
          s"""
  $groupId %% $artifactId
    stable: ${sbtStable.getOrElse("-")}
    others: ${others}"""
        }.mkString("\n")
  }

  /*
   * CLI
   */
  def main(args : Array[String]) : Unit = {
    if (args.length == 0 || args(0) == "--help") {
      printOptions()
      return
    }

    val argOptions = args.filter( _.startsWith("--"))
    for (argOption <- argOptions){
      if (!options.contains(argOption)) {
        println("Unknown option: " + argOption)
        printOptions()
        return
      }
    }

    // retrieves the ith + 1 entry after each given option
    def getOption(option : String) : String = {
      val index = args.indexOf(option)
      if (index != (-1)) {
        if (index + 1 >= args.length) return ""
        if (args(index + 1).startsWith("--")) return ""
        return args(index + 1)
      }
      return ""
    }

    // terminal options
    val fullyQualified = getOption("--fully-qualified")
    val group          = getOption("--group")
    val artifact       = getOption("--artifact")
    val version        = getOption("--version-number")
    val packaging      = getOption("--packaging")
    val classifier     = getOption("--classifier")
    val className      = getOption("--class")
    val scalaArg       = getOption("--scala-version")

    // forced versions flag coerces the return value to a coordinate (i.e versions as opposed to latest)
    val forceVersions  = if (args.indexOf("--force-versions") != (-1)) true else false

    val searchSuffixes = List("", "_2.10", "_2.11")

    val artifacts = (List.fill(searchSuffixes.length)(artifact), searchSuffixes).zipped map(_ + _)
    
    // construct string options
    val searchTerm = if (!fullyQualified.isEmpty) {
          String.format("fc:\"%s\"", fullyQualified)
      } else if (!className.isEmpty) {
          String.format("c:\"%s\"", className)
      } else if (!group.isEmpty() || !artifact.isEmpty) {
          showCoordinate(Coordinate (group, artifacts, version, packaging, className)).split(" ").toList.mkString(" AND ")
      } else {
        if (args(0).startsWith("--")) "" else args(0)
      }

    if (searchTerm == "") {
      println("Ill-formed search term")
      printOptions()
      return 
    }


    // remove basic because it requires return type to be either class/coord
    // assume basic search if no formating is done
    val basicSearch = (args.length == 1)
    
    // determine whether or not to perform a coordsearch (see README)
    val coordSearch = ((!group.isEmpty && !artifact.isEmpty && forceVersions) ||
      (!version.isEmpty) || (!className.isEmpty) || !fullyQualified.isEmpty)

    val searchFunction = if (basicSearch) (x : String) => basic(x) else {
        if (coordSearch || forceVersions) {
        (x : String) => byCoordinate(x, forceVersions)
      } else {
        (x : String) => byClassname(x)
      } 
    }

    val result = searchFunction(searchTerm)

    val stringResults = result match {
      case ClassVector(s) => showClassResults(s)
      case CoordVector(d) => showCoordResults(d)
    }

    if (stringResults.isEmpty) println("Search returned no results") else println(stringResults)
    
  }
}
