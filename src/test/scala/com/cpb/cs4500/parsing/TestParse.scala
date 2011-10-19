package com.cpb.cs4500.parsing {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.parsing._

  class TestParse extends FunSuite {
    //thingy vals
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
    val testSpecEquations = Equation("Equations:")
    val testSpec = Spec(testADTSignatureList, testSpecEquations)

    //moreComplicatedThingy vals
    val stringType = TypeLiteral("string")
    val charType = TypeLiteral("character")
    val makeDadOp = Operation("makeDad")
    val anotherMethodOp = Operation("anotherMethod")
    val evenAThirdMethodOp = Operation("evenAThirdMethod")
    val makeDadArgTypes = ArgTypes(List(intType, stringType))
    val anotherMethodArgTypes = ArgTypes(List())
    val evenAThirdMethodArgTypes = ArgTypes(List(stringType))
    val makeDadOpSpec = OperationSpec(makeDadOp, makeDadArgTypes, charType)
    val anotherMethodOpSpec = OperationSpec(anotherMethodOp, anotherMethodArgTypes, intType)
    val evenAThirdMethodOpSpec = OperationSpec(evenAThirdMethodOp, evenAThirdMethodArgTypes, booleanType)
    val anotherADTOperationSpecs = OperationSpecs(List(makeDadOpSpec, anotherMethodOpSpec, evenAThirdMethodOpSpec))
    val anotherADTName = "anotherADT"
    val anotherADT = TypeName(anotherADTName)
    val anotherADTSignature = ADTSignature(anotherADT, anotherADTOperationSpecs)
    val anotherADTSignatureList = ADTSignatures(List(anotherADTSignature))
    val anotherSpecEquations = Equation("Equations:")
    val anotherSpec = Spec(anotherADTSignatureList, anotherSpecEquations)

    val thingy = "Signatures: ADT: THISADT makeBool: int * int -> boolean Equations:"
    val moreComplicatedThingy = "Signatures: ADT: anotherADT makeDad: int * string -> character anotherMethod: -> int evenAThirdMethod string -> boolean Equations:"

    val parser = new ADTParser()

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

    test("testThisADTOperationSpecs") {
      expect(thisADTOperationSpecs) {     
        parser.parseAll(parser.operationSpecs, "makeBool: int * int -> boolean").get
      }
    }

    test("testThisADT") {
      expect(thisADT) { parser.parseAll(parser.typeName, "THISADT").get }
    }

    test("testThisADTSignature") {
      expect(thisADTSignature) {
        parser.parseAll(parser.adtSignature, "ADT: THISADT makeBool: int * int -> boolean").get
      }
    }

    test("testTestADTSignatureList") {
      expect(testADTSignatureList) {
        parser.parseAll(parser.adtSignatures, "ADT: THISADT makeBool: int * int -> boolean").get
      }
    }

    test("testTestSpecEquations") {
      expect(testSpecEquations) {
        parser.parseAll(parser.equations, "Equations:").get
      }
    }

    test("testTestSpec") {
      expect(testSpec) {
        parser.parseAll(parser.spec, thingy).get
      }
    }

//    test("testMoreComplicatedSpec") {
//      expect(anotherSpec) {
//        parser.parseAll(parser.spec, moreComplicatedThingy).get
//      }
//    }

  }

}
