package com.cpb.cs4500.parsing {

  import org.scalatest.FunSuite

  class TestParse extends FunSuite {
    val intType = TypeLiteral("int")
    val booleanType = TypeLiteral("boolean")
    val makeBoolOp = Operation("makeBool")
    val makeBoolArgTypes = ArgTypes(List(intType, intType))
    val makeBoolOpSpec = OperationSpec(makeBoolOp, makeBoolArgTypes, booleanType)
    val thisADTOperationSpecs = OperationSpecs(List(makeBoolOpSpec))
    val adtName = "THISADT"
    val thisADT = TypeName(adtName) 
    val thisADTSignature = ADTSignature(thisADT, thisADTOperationSpecs)
    val testADTSignatureList = ADTSignatures(List(thisADTSignature))
    val testSpecEquations = Equation("")
    val testSpec = Spec(testADTSignatureList, testSpecEquations)

    val thingy = "Signatures: ADT: THISADT makeBool: int * int -> boolean Equations:"
    val parser = new ADTParser()
    //val argTypes = List(ArgType("int"), ArgType("int"))
    //    val adtSigs = 
    test("testParse") {

      expect ("") { parser.parseAll(parser.spec, thingy) }
    }
    test("testIntType") {
      expect(intType) { parser.parseAll(parser.typeLiteral, "int").get }
    }

    test("testBooleanType") {
      expect(booleanType) { parser.parseAll(parser.typeLiteral, "boolean").get }
    }

    test("testMakeBoolOp") {
      expect(makeBoolOp) { parser.parseAll(parser.operation, "makeBool").get }
    }

    test("testMakeBoolArgTypes") {
      expect(makeBoolArgTypes) {
        parser.parseAll(parser.argTypes, "int * int").get
      }
    }

    test("testMakeBoolOpSpec") {
      expect(makeBoolOpSpec) { 
        parser.parseAll(parser.operationSpec, "makeBool: int * int -> boolean").get 
      }
    }

  }

}
