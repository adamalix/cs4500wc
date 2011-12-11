/**
 * Runner.scala
 * The main entry point for the software, launched from cs4500
 */

package com.cpb.cs4500 {
  import com.cpb.cs4500.io._
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.rewriting._
  import com.cpb.cs4500.util.SchTestConverter
  import com.cpb.cs4500.valueGeneration.ValueGenerator

  import scala.collection.immutable.ListSet

  object Runner {

    // Minimum number of tests to generate
    val minimumTests = 10

    def main(args: Array[String]) {
      val input: String = ReadWriter.inputFromFile(args(0))
      val parser = new ADTParser()

      // parse the input, if it passes, generate the tests and write to the file
      parser.parseAll(parser.spec, input) match {
        case parser.Success(result, _) => generateTerms(result, args(1))
        case parser.Failure(msg, _) => fail(msg)
        case parser.Error(msg, _) => error(msg)
      }
    }

    // Generate tests for this spec and write them to a file
    def generateTerms(spec: Spec, outfile: String) = {
      val valGen = new ValueGenerator(spec)
      val generatedValues = valGen.createAllTests(4)
      val rewriter = new Rewriter(spec)
      val termValuePairs = rewriter.rewriteTerms(generatedValues)
      // Generate the test strings
      var exprList = List[String]()
      var testCount = 0
      val opspecs = spec.getAllOpSpecs
      for (pair <- termValuePairs) {
        exprList = exprList :+ SchTestConverter.createTestSexpr(pair, testCount, opspecs)
        testCount += 1
      }
      ReadWriter.outputToFile(outfile, exprList, getAllADTNames(spec.getAllTypeNames))
    }


    def findOpSpecForOp(op: Operation, opSpecs: ListSet[OperationSpec]): OperationSpec = {
      for (opSpec <- opSpecs) {
        if (opSpec.op == op)
          return opSpec
      }
      throw new RuntimeException("Couldn't find OperationSpec for Operation")
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
