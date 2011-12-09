/*
 Runner.scala
 The main entry point for the software
 */
package com.cpb.cs4500 {

  import scala.collection.immutable.ListSet
  import scala.util.parsing.combinator._
  import com.cpb.cs4500.io._
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.rewriting._
  import com.cpb.cs4500.valueGeneration.ValueGenerator

  object Runner {

    val minimumTests = 10

    def main(args: Array[String]) {
      val input:String = ReadWriter.inputFromFile(args(0))
      val parser = new ADTParser()

      // parse the input, if it passes, generate the tests and write to the file
      parser.parseAll(parser.spec, input) match {
        case parser.Success(result, _) => generateTerms(result, args(1))
        case parser.Failure(msg, _) => fail(msg)
        case parser.Error(msg, _) => error(msg)
      }


    }

    //the generate magic happens here
    def generateTerms(spec: Spec, outfile: String) = {
      val valGen = new ValueGenerator(spec)
      val rewriter = new Rewriter(spec)
      var termValuePairs = ListSet[Tuple2[Term, Rhs]]()
      //generate the test strings
      var exprList = List[String]()
      var testCount = 0
      for (pair <- termValuePairs) {
        exprList = exprList :+ toTestSexpr(pair, testCount)
        testCount += 1
      }
      ReadWriter.outputToFile(outfile, exprList, getAllADTNames(spec.getAllTypeNames)) 
    }

    // converts the term and it's rewritten counterpart into a test expression
    def toTestSexpr(pair: Tuple2[Term, Rhs], count: Int): String = {
      "(test " + "\"test" + count + "\" " + 
        "(= " + pair._2.toSexpr + " " + pair._1.toSexpr + "))"
    }

    def fail(failureMessage: String) = {
      println("!!Failure:\nMalformed Input:\n" + failureMessage)
    }

    def error(errorMessage: String) = {
      println("error, sorry:\n" + errorMessage)
    }

    // helper function to get all the differnt ADT names from a spec
    def getAllADTNames(names: ListSet[TypeName]): List[String] = {
      var out = List[String]()
      for (name <- names) {
        out = out :+ name.toString
      }
      out
    }
  }
}
