package com.cpb.cs4500.rewriting {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.rewriting._
  import com.cpb.cs4500.parsing._

  class TestRewriter extends FunSuite {
    val testPhrase = "Signatures: ADT: THISADT makeBool: int * int -> boolean Equations:"
    val parser = new ADTParser()
    val rewriter:Rewriter = new Rewriter()
    test("RewriteTest1") {expect("ADT named: THISADT" + "\n" + "(test (makeBool int int) boolean)") 
                         {rewriter.applyRewriteRules(parser.parseAll(parser.spec, testPhrase).get)}}
  }
}