package com.cpb.cs4500.parsing {

  import org.scalatest.FunSuite

  class TestParse extends FunSuite {
    val thingy = "Signatures: ADT: THISADT makeBool: int * int -> boolean Equations:"
    val parser = new ADTParser()
    //val argTypes = List(ArgType("int"), ArgType("int"))
    //    val adtSigs = 
    test("testParse") {expect ("")
                       {parser.parseAll(parser.spec, thingy)}}
  }

}
