# MavenSearch
Prototype for the search component of a build tool.

# Usage
The most basic kind of search requres you to include some search query term without any extra options.
`scala MavenSearch slick`

Such a search will return all the packages that have that name either in the group or artifactId. The return value for a basic search is a ClassResult. ClassResults contain, as their output, the groupId (the package name minus the artifactId, eg com.workingmouse), the artifactId (roughly what you'd typically consider as the package name), and the latestVersion.

You can search using a fully qualified classname (groupId + artifactId) as follows:
`scala MavenSearch [-fc] [Fully Qualified Name]`

A fully qualified classname search returns a vector containing the the groupId, artifactId and version of the package.
The combination of these three return values is referred to in the API as a Coordinate result. 

Alternatively you can search by Classname as follows:
`scala MavenSearch [-c] [Class Name]`

You can also run a compound search. Say for example you wanted to version 3.0 of org.workingmouse's scalaz package. The query would be:
`scala MavenSearch -g org.workingmouse -a scalaz -v 3.0`

This search returns a CoordinateResult. Leaving out `-v 3.0` from the search query would return a result of type ClassnameResult.
