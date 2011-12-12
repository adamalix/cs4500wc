package com.cpb.cs4500.util {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.io.ReadWriter

  import org.scalatest.FunSuite

  class TestSchTestConverter extends FunSuite {

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


    /*
     Functions to potentially test:
     * - createTestSexpr
     * - tupleToTest
     * - findWrapperForOp
     * - findSchConverter
     * - getOpName
    */

    // this function is pretty simple so i'm not going to test it
    // too exhaustively
    test("test getOpName") {
      // takes a term, outputs a string
      val op = Operation("testOp")
      val args = EmptyArg()
      val testTerm = TermExpr(op, args)

      val testTermID = TermID("hey")

      expect("testOp") { SchTestConverter.getOpName(testTerm)   }
      expect("")       { SchTestConverter.getOpName(testTermID) }
    }

    test("test findWrapperForOp") {
      // takes and op and a listset of opspecs and returns a string
    }

    test("test findSchConverter") {
      // takes a return type(terminal) and a listset of opSpecs
      // returns a string that represents the proper scheme converter
      val spec5 = parser.parseAll(parser.spec, testFile5).get
      val opspecsListSet = spec5.getAllOpSpecs
      val opspecs = opspecsListSet.toList
      val zeroSpec = opspecs.last

      // construct each literal
      val boolRet = BooleanLiteral(true, "true")
      val charRet = CharLiteral('c', "c")
      val stringRet = StringLiteral("str")
      val intRet = IntLiteral(1, "1")

      val natType = TypeName("Nat")

      expect("(asInt ") {
       // SchTestConverter.findSchConverter(zeroSpec.returnType, opspecsListSet)
      }

      expect("(= ") {
       // SchTestConverter.findSchConverter(intRet, opspecsListSet)
      }

      expect("bool") {
       // SchTestConverter.findSchConverter(boolRet, opspecsListSet)
      }

      expect("(char=? ") {
        // SchTestConverter.findSchConverter(charRet, opspecsListSet)
      }

      expect("(string=? ") {
        // SchTestConverter.findSchConverter(stringRet, opspecsListSet)
      }

      expect("(asInt ") {
        // SchTestConverter.findSchConverter(natType, opspecsListSet)
      }
    }

  }
}
