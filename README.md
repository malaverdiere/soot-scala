# soot-scala
Scala wrappers for Soot and other utilities.

It features the following:

 * ScalaWrappers: many implicit classes that offer methods that follow the Scala idioms over major Soot classes.
 * SootAnnotationUtils: tools to deal with annotations (esp. Java annotations) in Soot
 * Logging: A minimalistic logging library wrapper over SLF4J


The SBT build file is purposefully incomplete. This project should be part of a multi-project
build, whereby the main SBT build will add the dependency to Soot.

The Maven pom file has a dependency on a version of Soot that doesn't exist yet.
That is normal and intended to be part of a multi-module build along the same lines as above.