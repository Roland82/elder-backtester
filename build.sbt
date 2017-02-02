name := """elder-trader-bot"""

version := "1.0"

scalaVersion := "2.11.8"

// Development
libraryDependencies += "org.scalaz" % "scalaz-core_2.11"  % "7.3.0-M8"
libraryDependencies += "org.scalaz" % "scalaz-concurrent_2.11"  % "7.3.0-M8"
libraryDependencies += "joda-time"  % "joda-time"         % "2.9.7"

// Test
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"



fork in run := true
coverageEnabled := true