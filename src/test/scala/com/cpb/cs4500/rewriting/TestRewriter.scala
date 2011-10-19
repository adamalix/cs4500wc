package com.cpb.cs4500.rewriting {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.rewriting._
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.io._

  class TestRewriter extends FunSuite {
    val testPhraseYay:String =  "Signatures: ADT: THISADT makeBool: int * int -> boolean " +
                                                          "huh: -> boolean " +
                                                          "yoyo: int * int * int -> int " +
                                            "ADT: DATADT testWoop: string * int -> char " +
                                            "ADT: coolStory Foobar: foo * lolz * kek * char -> WATISDIS " +
                                "Equations:"
   
    val parser = new ADTParser()
    val rewriter:Rewriter = new Rewriter()
    //val input:String = ReadWriter.inputFromFile("C:\\Users\\Paul\\Documents\\School\\Fall 2011\\Software Dev\\cs4500\\src\\main\\resources\\testInput.txt")
    //ReadWriter.outputToFile("C:\\Users\\Paul\\Documents\\School\\Fall 2011\\Software Dev\\cs4500\\src\\main\\resources\\lolz.txt", rewriter.applyRewriteRules(parser.parseAll(parser.spec, input).get))
    // ReadWriter.outputToFile("meow.txt", rewriter.applyRewriteRules(parser.parseAll(parser.spec, input).get))
    test("RewriteTest1") {expect("ADT named: THISADT" + "\n" + "(test (makeBool int int) boolean)") 
                         {rewriter.applyRewriteRules(parser.parseAll(parser.spec, testPhraseYay).get)}}
  }
}