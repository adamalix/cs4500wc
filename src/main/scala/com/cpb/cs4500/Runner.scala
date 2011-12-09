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

    //the generate magic happens here
    def generateTerms(spec: Spec) = {
      val valGen = new ValueGenerator(spec)
      val rewriter = new Rewriter(spec)
      var termValuePairs = ListSet[Tuple2[Term, Rhs]]()
      //PQ: commented this line out because i changed the implementation of rewrite
      //termList.foreach((term: Term) => termList += (term, rewriter.rewriteTerm(term)))

      //need to rewrite terms into scheme expressions as strings and then send them to the file outputter
      var exprList = List[String]()
      for (pair <- termValuePairs) {
        exprList = exprList :+ toTestSexpr(pair)
      }
    }

    def toTestSexpr(pair: Tuple2[Term, Rhs]): String = {
      "(= " + pair._2.toSexpr + " " + pair._1.toSexpr + ")"
    }

    def fail(failureMessage: String) = {
      println("!!Failure:\nMalformed Input:\n" + failureMessage)
    }

    def error(errorMessage: String) = {
      println("error, sorry:\n" + errorMessage)
    }

  }
}
