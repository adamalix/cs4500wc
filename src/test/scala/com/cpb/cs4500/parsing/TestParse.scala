package com.cpb.cs4500.parsing {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.parsing._

  class TestParse extends FunSuite {
    val thingy = "Signatures: ADT: THISADT makeBool: int * int -> boolean Equations:"
    val parser = new ADTParser()
    //val argTypes = List(ArgType("int"), ArgType("int"))
    //    val ad
    test("testParse") {expect ("")
                       {parser.parseAll(parser.spec, thingy).get.toString}}
                       
  }

}
