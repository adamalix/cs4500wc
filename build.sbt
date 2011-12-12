// CPB.cs4500 build settings
name    := "cs4500"

version := "0.8"

organization := "com.cpb"

scalaVersion := "2.9.1"

// set the main Scala source directory to be <base>/src
scalaSource in Compile <<= baseDirectory(_ / "src")

// set the Scala test source directory to be <base>/test
scalaSource in Test <<= baseDirectory(_ / "src/test")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1"
