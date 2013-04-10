organization := "edu.washington.cs.knowitall.ollie"

name := "ollie-core"

description := "Wrapper and implementation for extractors of chunked sentences."

version := "1.0.4-SNAPSHOT"

crossScalaVersions := Seq("2.9.2", "2.10.1")

scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head }

libraryDependencies ++= Seq(
    "edu.washington.cs.knowitall.nlptools" %% "nlptools-core" % "2.4.1",
    "edu.washington.cs.knowitall.nlptools" %% "nlptools-conf-breeze" % "2.4.1",
    "edu.washington.cs.knowitall.nlptools" %% "nlptools-stem-morpha" % "2.4.1",
    "org.slf4j" % "slf4j-api" % "1.7.2",
    "org.scalaz" %% "scalaz-core" % "6.0.4",
    "ch.qos.logback" % "logback-classic" % "1.0.9" % "test",
    "ch.qos.logback" % "logback-core" % "1.0.9" % "test",
    "junit" % "junit" % "4.11" % "test",
    "org.specs2" %% "specs2" % "1.12.3" % "test")

scalacOptions ++= Seq("-unchecked", "-deprecation")

licenses := Seq("Ollie Software License Agreement" -> url("https://raw.github.com/knowitall/ollie/master/LICENSE"))

homepage := Some(url("http://ollie.cs.washington.edu"))

publishMavenStyle := true

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
  <scm>
    <url>https://github.com/knowitall/ollie</url>
    <connection>scm:git://github.com/knowitall/ollie.git</connection>
    <developerConnection>scm:git:git@github.com:knowitall/ollie.git</developerConnection>
    <tag>HEAD</tag>
  </scm>
  <developers>
   <developer>
      <name>Michael Schmitz</name>
    </developer>
    <developer>
      <name>Robert Bart</name>
    </developer>
  </developers>)
