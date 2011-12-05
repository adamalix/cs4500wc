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

      parser.parseAll(parser.spec, input) match {
        case parser.Success(result, _) => generateTerms(result)
        case parser.Failure(msg, _) => fail(msg)
        case parser.Error(msg, _) => error(msg)
      }
    }

    def generateTerms(spec: Spec) = {
      val valGen = new ValueGenerator(spec)
      val rewriter = new Rewriter(spec)
      var termList: ListSet[Term] = valGen.generateTerms(minimumTests)
      var termValuePairs = ListSet[Tuple2[Term, Rhs]]()
      termList.foreach((term: Term) => termList += (term, rewriter.rewriteTerm(term)))
    }

    def fail(failureMessage: String) = {
      println("!!Failure:\nMalformed Input:\n" + failureMessage)
    }

    def error(errorMessage: String) = {
      println("error, sorry:\n" + errorMessage)
    }

  }
}
