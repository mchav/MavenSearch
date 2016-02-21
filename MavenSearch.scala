// TODO: use scopt as an Option parser

import java.net._ 
import java.text.SimpleDateFormat
import java.time._ 
import java.util.Date
import java.util.Scanner
import scala.collection.immutable
import scala.util.matching.Regex
import scala.util.parsing.json._ 

object MavenSearch {
    // type synonyms to make the code more readable
    type ClassName    = String
    type GroupID      = String
    type ArtifactID   = String
    type Version      = String
    type Timestamp    = LocalDateTime
    type VersionCount = Double
    type PackageMap   = Map[String, Any]

    case class CoordinateResult(
        groupId: GroupID,
        artifactId: ArtifactID,
        version: Version,
        timestamp: Timestamp
    )

    case class ClassnameResult(
        groupId: GroupID,
        artifactId: ArtifactID,
        latestVersion: Version,
        timestamp: LocalDateTime,
        versionCount: Double
    )

    // toString?
    trait CoordinateString {
        override def toString() : String = {
            return  ""
        }
    }

    case class Coordinate(
        groupId: GroupID,
        artifactId: ArtifactID,
        version: Version,
        packaging: String,
        classifier: Version
    )

    trait QueryType
    case class BasicQuery (packageName: String) extends QueryType
    case class CoordinateQuery(coordinates: Coordinate) extends QueryType
    case class ClassQuery(className: String) extends QueryType
    case class FullQuery(fullName: String) extends QueryType


    val baseUrl = "http://search.maven.org/solrsearch/select?"
    val charset = java.nio.charset.StandardCharsets.UTF_8.name()

    def intercalate(words : List[String], delimiter : String) : String = {
      return words match {
        case Nil     => ""
        case List(x) => x
        case x :: xs => x + " " + delimiter + " " + intercalate(xs, delimiter)
      }
    }

    // hack for toString
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

    def constructURL (searchTerm : QueryType) : String = {
        val queryString = searchTerm match {
            case BasicQuery(b)       => b
            case CoordinateQuery(c)  => intercalate(showCoordinate(c).split(" ").toList, "AND")
            case ClassQuery(c)       => String.format("c:\"%s\"", c)
            case FullQuery(c)        => String.format("fc:\"%s\"", c)
        }

        // defaults
        val rows = "20"
        val wt = "json"

        val complete = String.format(baseUrl + "q=%s&rows=%s&wt=%s", 
           URLEncoder.encode(queryString, charset), 
           URLEncoder.encode(rows, charset),
           URLEncoder.encode(wt, charset))
        println(complete)
        complete
    }

    def timestampToDate(time: Double) : Timestamp = {
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
            packageMap("v").asInstanceOf[String], 
            timestampToDate(packageMap("timestamp").asInstanceOf[Double])
        )  
    }

    def basicSearch(query: QueryType) : Vector[ClassnameResult] = {
        val url = constructURL(query)
        val packageMaps = downloadPackageInfo(url)
        packageMaps.map(pkg => parseClassResult(pkg)).to[Vector]
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

    /*
    example requests
    http://search.maven.org/solrsearch/select?q=g:"joda-time"&rows=100&wt=json&core=gav
    http://search.maven.org/solrsearch/select?q=fc:"org.joda.time.DateTime"&rows=100&wt=json
    */
    def byCoordinate(query : QueryType): Vector[CoordinateResult] = {
        val url = constructURL(query)
        val packageMaps = downloadPackageInfo(url)
        packageMaps.map(pkg => parseCoordinateResult(pkg)).to[Vector]
    }

    def byFullClassname(query : QueryType): Vector[CoordinateResult] = {
        byCoordinate(query)
    }

    /*
    example requests
    http://search.maven.org/solrsearch/select?q=g:"joda-time"&rows=100&wt=json
    */
    def byClassname(query : QueryType): Vector[ClassnameResult] = {
        val url = constructURL(query)
        val packageMaps = downloadPackageInfo(url)
        packageMaps.map(pkg => parseClassResult(pkg)).to[Vector]
    }

    def parseQuery(queryList : Array[String]) : QueryType = {
        def getOption(option : String) : String = {
            val index = queryList.indexOf(option)
            if (index != (-1)) 
                return queryList(index + 1) 
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

        if (fc != "") {
            return FullQuery(fc)
        } else if (c != "") {
            return ClassQuery(c)
        } else if (g != "" || id != "" || a != "") {
            return CoordinateQuery( Coordinate (g, a, v, p, c))
        } else {
            return BasicQuery(queryList(0))
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

        val searchQuery = parseQuery(args)

        val result =  searchQuery match {
            case BasicQuery(b)       => basicSearch(searchQuery)
            case CoordinateQuery(c)  => byCoordinate(searchQuery)
            case ClassQuery(c)       => byClassname(searchQuery)
            case FullQuery(c)        => byFullClassname(searchQuery)
        }
        result.map (x => println(x))
    }
}