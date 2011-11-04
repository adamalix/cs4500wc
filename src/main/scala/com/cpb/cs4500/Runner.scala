/*
 Runner.scala
 The main entry point for the software
 */
package com.cpb.cs4500 {
  
  import scala.util.parsing.combinator._
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.rewriting._
  import com.cpb.cs4500.io._

  object Runner {

    def main(args: Array[String]) {
      val input:String = ReadWriter.inputFromFile(args(0))
      val parser = new ADTParser()
      val rewriter:Rewriter = new Rewriter()

      parser.parseAll(parser.spec, input) match {
        case parser.Success(result, _) => ReadWriter.outputToFile(args(1), rewriter.applyRewriteRules(result))
        case parser.Failure(msg, _) => ReadWriter.outputToFile(args(1), "!!FAILURE!!:\n" + msg) 
        case parser.Error(_, _) => ReadWriter.outputToFile(args(1), "error, sorry.")
      }
    }
  }
}
