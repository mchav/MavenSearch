object Test extends App {
  // testing against search.maven.org results
  assert(!MavenSearch.byCoordinate("c:\"scalaz\"", false).isEmpty)
}
