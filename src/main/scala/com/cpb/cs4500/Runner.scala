package com.cpb.cs4500{

    object Runner extends Application
    {
        import com.cpb.cs4500.parsing._
        import com.cpb.cs4500.rewriting._
        import com.cpb.cs4500.io._
        
        override def main(args: Array[String]) {
            val input:String = ReadWriter.inputFromFile("test.txt")
            val parser = new ADTParser()
            val rewriter:Rewriter = new Rewriter(parser.parseAll(parser.spec, input).get)
            ReadWriter.outputToFile("meow.txt", rewriter.applyRewriteRules)
        }
    }
}