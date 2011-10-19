package com.cpb.cs4500.rewriting {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.rewriting._
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.io._

  class TestRewriter extends FunSuite {
    val testPhraseYay:String =  "Signatures: ADT: THISADT makeBool: int * int -> boolean " +
                                                          "huh: -> boolean " + 
                                            "ADT: DATADT testWoop: string * int -> char " +
                                            "ADT: coolStory Foobar: foo * lolz * kek * char -> WATISDIS " +
                                "Equations:"
   
    val parser = new ADTParser()
    val rewriter:Rewriter = new Rewriter()
    println(System.getProperty("user.dir"))
    val input:String = ReadWriter.inputFromFile("../../../../../../../src/test/resources/testInput.txt")
    println("INPUT: " + input)
    ReadWriter.outputToFile("meow.txt", rewriter.applyRewriteRules(parser.parseAll(parser.spec, input).get))
    test("RewriteTest1") {expect("ADT named: THISADT" + "\n" + "(test (makeBool int int) boolean)") 
                         {rewriter.applyRewriteRules(parser.parseAll(parser.spec, testPhraseYay).get)}}
  }
}