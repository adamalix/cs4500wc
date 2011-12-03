/*
TestParse.scala
Holds the test cases for the parsing mechanism of our software
*/
package com.cpb.cs4500.parsing {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.parsing._

  class TestParse extends FunSuite {
    //thingy vals
    val intType = IntLiteral("int")
    val booleanType = BooleanLiteral("boolean")
    val makeBoolOp = Operation("makeBool")
    val makeBoolArgTypes = ArgTypes(List(intType, intType))
    val makeBoolOpSpec = OperationSpec(makeBoolOp, makeBoolArgTypes, booleanType, false)
    val thisADTOperationSpecs = OperationSpecs(List(makeBoolOpSpec))
    val adtName = "THISADT"
    val thisADT = TypeName(adtName) 
    val thisADTSignature = ADTSignature(thisADT, thisADTOperationSpecs)
    val testADTSignatureList = ADTSignatures(List(thisADTSignature))
    val term = Term("", Operation(""), Arg())
    val testSpecEquations = Equations(List(Equation(term, term)))
    val emptyEquations = Equations(List())
    val testSpec = Spec(testADTSignatureList, emptyEquations)

    //moreComplicatedThingy vals
    val stringType = StringLiteral("string")
    val charType = CharLiteral("character")
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
    val anotherSpecEquations = Equations(List(Equation(term, term)))

    //moreComplicatedThingy equations
    val emptyOp = Operation("")
    val asdfTerm = Term("asdf", emptyOp, Arg())
    val dadTerm = Term("dad", emptyOp, Arg())
    val lolTerm = Term("lol", emptyOp, Arg())

    val eq1Left = Term("", makeDadOp, Args(asdfTerm, Args(dadTerm, Arg())))
    val eq1Right = Term("", evenAThirdMethodOp, Args(lolTerm, Arg()))
    val eq1 = Equation(eq1Left, eq1Right)
    val moreComplicatedThingyEquations = Equations(List(eq1))

    val anotherSpec = Spec(anotherADTSignatureList, moreComplicatedThingyEquations)

    val thingy = "Signatures: ADT: THISADT makeBool: int * int -> boolean Equations:"
    val moreComplicatedThingy = "Signatures: ADT: anotherADT makeDad: int * string -> character anotherMethod: -> int evenAThirdMethod: string -> boolean Equations: (makeDad adsf dad) = (evenAThirdMethod lol)"

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
        parser.parseAll(parser.term, "(evenAThirdMethod lol)") match {
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
      expect(anotherSpec) {
        parser.parseAll(parser.spec, moreComplicatedThingy) match {
          case parser.Success(result, _) => result
          case parser.Failure(msg, _) => msg
          case parser.Error(_, _) => "error, sorry."
	}
       
      }
    }

//    def handleParsing(pr: parser.ParseResult[Any]) = pr match {
//      case pr.Success(result, _) => result
//      case pr.Failure(msg, _) => msg
//      case pr.Error(_, _) => "error"
//    }

  }

}
