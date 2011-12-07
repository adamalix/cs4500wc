package com.cpb.cs4500.valueGeneration {
  import org.scalatest.FunSuite
  import com.cpb.cs4500.io.ReadWriter
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._

  import scala.collection.immutable.ListSet

  class TestGeneration extends FunSuite {
    val parser = new ADTParser()
    val testFileName = "src/test/resources/test5"
    val testFile = ReadWriter.inputFromFile(testFileName)
    val spec = parser.parseAll(parser.spec, testFile)

    /*
    Functions to test:
    - createAllTests
    - makeListOfArgs
    - cart
    - createBasicCreatorMap
    */

    test("test createAllTests"){
        
    }
    
    test("test makeListOfArgs"){
        
    }

    test("test createBasicCreatorMap"){
        
    }

    test("test cart"){
        
    }
  }
}

//express the idea that using this update would result in making the software much more robust, save us work, and requires little effort on the part of the client (is package)
