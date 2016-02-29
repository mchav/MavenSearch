# MavenSearch
Prototype for the search component of a build tool. The project uses no additional dependencies. Option parsing is done manually and JSON parsing is done using the standard Scala library. The output is printed such that the results can easily be copied and pasted to sbt or any similar build tool.

# Usage
`maven_search [option] [argument]`

Options:

--fully-qualified  : search by fully qualified package name

--class            : search by class name

--group            : search by groupId

--artifact         : search by artifactID

--version-number   : search by version number (used in conjunction with other options)

--packaging        : search by packaging (*.jar or *.pom)

--classifier       : search by classifier


# Detailed explanation
The most basic kind of search requres you to include some search query term without any extra options.


`maven_search slick`

Such a search will return all the packages that have that name either in the group or artifactId. The return value for a basic search is a ClassResult. ClassResults contain, as their output, the groupId (the package name minus the artifactId, eg com.workingmouse), the artifactId (roughly what you'd typically consider as the package name), and the latestVersion.

You can search using a fully qualified classname (groupId + artifactId) as follows:


`maven_search [--fully-qualified] [Fully Qualified Name]`

A fully qualified classname search returns a vector containing the the groupId, artifactId and version of the package.
The combination of these three return values is referred to in the API as a Coordinate result. 

Alternatively you can search by Classname as follows:


`maven_search [--class] [Class Name]`

You can also run a compound search. Say for example you wanted to version 3.0 of org.workingmouse's scalaz package. The query would be:


`maven_search --group org.workingmouse --artifact scalaz --version-number 3.0`
Returns:

  `com.workingmouse %% scalaz
    stable: "com.workingmouse" %% scalaz % "3.0"
    others: 2.5, 2.4, 2.3, 2.2`


This search returns a CoordinateResult. Leaving out `-version-number 3.0` from the search query would return a result of type ClassnameResult.

You can coerce a search to return a coordinate result by enabling the flag `--force-versions`
