package com.cpb.cs4500.parsing {


  import java.io.FileReader
  import org.scalatest.FunSuite

  class TestParse extends FunSuite {
     val thingy = "Signatures: ADT: THISADT makeBool: int -> boolean Equations:"
     val parser = new ADTParser()
     test("testParse") {expect ("")
                       {parser.parseAll(parser.spec, thingy)}}
  }
}
