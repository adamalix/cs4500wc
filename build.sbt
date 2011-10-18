// CPB.cs4500 build settings
name    := "cs4500"

version := "0.1"

organization := "com.cpb"

// set the main Scala source directory to be <base>/src
scalaSource in Compile <<= baseDirectory(_ / "src")

// set the Scala test source directory to be <base>/test
scalaSource in Test <<= baseDirectory(_ / "test")
