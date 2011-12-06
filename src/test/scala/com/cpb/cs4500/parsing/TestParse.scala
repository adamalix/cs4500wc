/*
 TestParse.scala
 Holds the test cases for the parsing mechanism of our software
 */
package com.cpb.cs4500.parsing {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.io.ReadWriter
  import com.cpb.cs4500.parsing._

  class TestParse extends FunSuite {
    //thingy vals
    val intType = new IntLiteral(-6789, "int")
    val booleanType = new BooleanLiteral(false, "boolean")
    val makeBoolOp = Operation("makeBool")
    val makeBoolArgTypes = ArgTypes(List(intType, intType))
    val makeBoolOpSpec = OperationSpec(makeBoolOp, makeBoolArgTypes, booleanType, false)
    val thisADTOperationSpecs = OperationSpecs(List(makeBoolOpSpec))
    val adtName = "THISADT"
    val thisADT = TypeName(adtName)
    val thisADTSignature = ADTSignature(thisADT, thisADTOperationSpecs)
    val testADTSignatureList = ADTSignatures(List(thisADTSignature))
    val termExpr = TermExpr(Operation(""), EmptyArg())
    val rhsExpr = RhsExpr(Operation(""), RhsEmptyArg())
    val testSpecEquations = Equations(List(Equation(termExpr, rhsExpr)))
    val emptyEquations = Equations(List())
    val testSpec = Spec(testADTSignatureList, emptyEquations)

    //moreComplicatedThingy vals
    val stringType = StringLiteral("string")
    val charType = new CharLiteral('c', "character")
    val makeDadOp = Operation("makeDad")
    val anotherMethodOp = Operation("anotherMethod")
    val evenAThirdMethodOp = Operation("evenAThirdMethod")
    val makeDadArgTypes = ArgTypes(List(intType, stringType))
    val anotherMethodArgTypes = ArgTypes(List())
    val evenAThirdMethodArgTypes = ArgTypes(List(stringType))
    val makeDadOpSpec = OperationSpec(makeDadOp, makeDadArgTypes, charType, false)
    //basic creator
    val anotherMethodOpSpec = OperationSpec(anotherMethodOp, anotherMethodArgTypes, intType, true)
    val evenAThirdMethodOpSpec = OperationSpec(evenAThirdMethodOp, evenAThirdMethodArgTypes, booleanType, false)
    val anotherADTOperationSpecs = OperationSpecs(List(makeDadOpSpec, anotherMethodOpSpec, evenAThirdMethodOpSpec))
    val anotherADTName = "anotherADT"
    val anotherADT = TypeName(anotherADTName)
    val anotherADTSignature = ADTSignature(anotherADT, anotherADTOperationSpecs)
    val anotherADTSignatureList = ADTSignatures(List(anotherADTSignature))
    val anotherSpecEquations = Equations(List(Equation(termExpr, rhsExpr)))

    //moreComplicatedThingy equations
    val emptyOp = Operation("")
    val asdfTerm = TermID("asdf")
    val dadTerm = TermID("dad")
    val lolTerm = TermID("lol")

    val eq1Left = TermExpr(makeDadOp, Args(asdfTerm, Args(dadTerm, EmptyArg())))
    val eq1Right = RhsExpr(evenAThirdMethodOp, RhsArgs(RhsID("lol"), RhsEmptyArg()))
    val eq1 = Equation(eq1Left, eq1Right)
    val moreComplicatedThingyEquations = Equations(List(eq1))

    val anotherSpec = Spec(anotherADTSignatureList, moreComplicatedThingyEquations)

    val thingy = "Signatures: ADT: THISADT makeBool: int * int -> boolean Equations:"
    val moreComplicatedThingy = "Signatures: ADT: anotherADT makeDad: int * string -> character anotherMethod: -> int evenAThirdMethod: string -> boolean Equations: (makeDad adsf dad) = (evenAThirdMethod lol)"

    val parser = new ADTParser()

    val testFileName1 = "src/test/resources/test1"
    val testFileName2 = "src/test/resources/test2"
    val testFileName3 = "src/test/resources/test3"
    val testFileName4 = "src/test/resources/test4"
    val testFileName5 = "src/test/resources/test5"
    val testFileName6 = "src/test/resources/test6"
    val testFileName7 = "src/test/resources/test7"

    val testFile1 = ReadWriter.inputFromFile(testFileName1)
    val testFile2 = ReadWriter.inputFromFile(testFileName2)
    val testFile3 = ReadWriter.inputFromFile(testFileName3)
    val testFile4 = ReadWriter.inputFromFile(testFileName4)
    val testFile5 = ReadWriter.inputFromFile(testFileName5)
    val testFile6 = ReadWriter.inputFromFile(testFileName6)
    val testFile7 = ReadWriter.inputFromFile(testFileName7)

    val testFileList = List(testFile1, testFile2, testFile3, testFile4, testFile5, testFile6, testFile7)

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
      expect(emptyEquations) {
        parser.parseAll(parser.equations, "").get
      }
    }

    test("testTestSpec") {
      expect(testSpec) {
        parser.parseAll(parser.spec, thingy).get
      }
    }

    test("testStringType") {
      expect(stringType) {
        parser.parseAll(parser.typeLiteral, "string").get
      }
    }

    test("testCharacterType") {
      expect(charType) {
        parser.parseAll(parser.typeLiteral, "character").get
      }
    }

    test("testMakeDadOp") {
      expect(makeDadOp) {
        parser.parseAll(parser.operation, "makeDad").get
      }
    }

    test("testAnotherMethodOp") {
      expect(anotherMethodOp) {
        parser.parseAll(parser.operation, "anotherMethod").get
      }
    }

    test("testEvenAThirdMethodOp") {
      expect(evenAThirdMethodOp) {
        parser.parseAll(parser.operation, "evenAThirdMethod").get
      }
    }

    test("testMakeDadArgTypes") {
      expect(makeDadArgTypes) {
        parser.parseAll(parser.argTypes, "int * string").get
      }
    }

    test("testAnotherMethodArgTypes") {
      expect(anotherMethodArgTypes) {
        parser.parseAll(parser.argTypes, "").get
      }
    }

    test("testEvenAThirdMethodArgTypes") {
      expect(evenAThirdMethodArgTypes) {
        parser.parseAll(parser.argTypes, "string").get
      }
    }

    test("testMakeDadOpSpec") {
      expect(makeDadOpSpec) {
        parser.parseAll(parser.operationSpec, "makeDad: int * string -> character").get
      }
    }

    test("testAnotherMethodOpSpec") {
      expect(anotherMethodOpSpec) {
        parser.parseAll(parser.operationSpec, "anotherMethod: -> int").get
      }
    }

    test("testEvenAThirdMethodOpSpec") {
      expect(evenAThirdMethodOpSpec) {
        parser.parseAll(parser.operationSpec, "evenAThirdMethod: string -> boolean").get
      }
    }

    test("testAnotherADTOperationSpecs") {
      expect(anotherADTOperationSpecs) {
        parser.parseAll(parser.operationSpecs, "makeDad: int * string -> character anotherMethod: -> int evenAThirdMethod: string -> boolean").get
      }
    }

    test("testAnotherADT") {
      expect(anotherADT) { parser.parseAll(parser.typeName, "anotherADT").get }
    }

    test("testAnotherADTSignature") {
      expect(anotherADTSignature) {
        parser.parseAll(parser.adtSignature, "ADT: anotherADT makeDad: int * string -> character anotherMethod: -> int evenAThirdMethod: string -> boolean").get
      }
    }

    test("testAnotherADTSignatureList") {
      expect(anotherADTSignatureList) {
        parser.parseAll(parser.adtSignatures, "ADT: anotherADT makeDad: int * string -> character anotherMethod: -> int evenAThirdMethod: string -> boolean").get
      }
    }

    test("testAsdfTerm") {
      expect(asdfTerm) {
        parser.parseAll(parser.term, "asdf") match {
          case parser.Success(result, _) => result
          case parser.Failure(msg, _) => "FAILURE: " + msg
          case parser.Error(_, _) => "error, sorry."
        }
      }
    }

    test("testDadTerm") {
      expect(dadTerm) {
        parser.parseAll(parser.term, "dad") match {
          case parser.Success(result, _) => result
          case parser.Failure(msg, _) => "FAILURE: " + msg
          case parser.Error(_, _) => "error, sorry."
        }
      }
    }

    test("testLolTerm") {
      expect(lolTerm) {
        parser.parseAll(parser.term, "lol") match {
          case parser.Success(result, _) => result
          case parser.Failure(msg, _) => "FAILURE: " + msg
          case parser.Error(_, _) => "error, sorry."
        }
      }
    }

    test("testEq1Left") {
      expect(eq1Left) {
        parser.parseAll(parser.term, "(makeDad asdf dad)") match {
          case parser.Success(result, _) => result
          case parser.Failure(msg, _) => "FAILURE: " + msg
          case parser.Error(_, _) => "error, sorry."
        }
      }
    }

    test("testEq1Right") {
      expect(eq1Right) {
        parser.parseAll(parser.rhs, "(evenAThirdMethod lol)") match {
          case parser.Success(result, _) => result
          case parser.Failure(msg, _) => "FAILURE: " + msg
          case parser.Error(_, _) => "error, sorry."
        }
      }
    }

    test("testEq1") {
      expect(eq1) {
        parser.parseAll(parser.equation, "(makeDad asdf dad) = (evenAThirdMethod lol)") match {
          case parser.Success(result, _) => result
          case parser.Failure(msg, _) => "FAILURE: " + msg
          case parser.Error(_, _) => "error, sorry."
        }
      }
    }

    test("testMoreComplicatedThingyEquations") {
      expect(moreComplicatedThingyEquations) {
        parser.parseAll(parser.equations, "(makeDad asdf dad) = (evenAThirdMethod lol)") match {
          case parser.Success(result, _) => result
          case parser.Failure(msg, _) => "FAILURE: " + msg
          case parser.Error(_, _) => "error, sorry."
        }
      }
    }

    test("testMoreComplicatedSpec") {
      expect(anotherSpec.toString()) {
        parser.parseAll(parser.spec, moreComplicatedThingy) match {
          case parser.Success(result, _) => result.toString()
          case parser.Failure(msg, _) => msg
          case parser.Error(_, _) => "error, sorry."
        }

      }
    }

    test("parseTestSpecs") {
      for (testFile <- testFileList) {
        expect(true) {
          parser.parseAll(parser.spec, testFile) match {
            case parser.Success(result, _) => true
            case _ => false
          }
        }
      }
    }

  }

}
