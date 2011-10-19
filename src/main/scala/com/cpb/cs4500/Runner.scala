/*
Runner.scala
The main entry point for the software
*/
package com.cpb.cs4500{


    object Runner extends Application
    {
        import com.cpb.cs4500.parsing._
        import com.cpb.cs4500.rewriting._
        import com.cpb.cs4500.io._
        
        override def main(args: Array[String]) {
            val input:String = ReadWriter.inputFromFile(args(0))
            val parser = new ADTParser()
            val rewriter:Rewriter = new Rewriter()
            ReadWriter.outputToFile(args(1), rewriter.applyRewriteRules(parser.parseAll(parser.spec, input).get))
        }
    }
}
