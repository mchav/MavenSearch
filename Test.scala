package chavxo.maven_search

object Test extends App {
  // testing against search.maven.org results
  println(MavenSearch.byCoordinate("c:\"scalaz\"", false))
}