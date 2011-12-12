package com.cpb.cs4500.util {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.io.ReadWriter
  import scala.collection.immutable.ListSet

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
     * x evaluatePrimExpr
     * - wrapRhsExpr
     * - wrapTerm
     * x getReturnType (term and rhs)
     * - findRhsWrapper
     * - findTermWrapper
     * x getOpName
    */

    test("test evaluatePrimExpr") {
      val rhsEmpty = RhsEmptyArg()
      val notExpr = RhsPrimExpr(Not(), rhsEmpty)
      val eqExpr = RhsPrimExpr(Eq(), rhsEmpty)
      val plusExpr = RhsPrimExpr(Plus(), rhsEmpty)
      val starExpr = RhsPrimExpr(Star(), rhsEmpty)
      val minusExpr = RhsPrimExpr(Minus(), rhsEmpty)
      val greaterThanExpr = RhsPrimExpr(GreaterThan(), rhsEmpty)
      val lessThanExpr = RhsPrimExpr(LessThan(), rhsEmpty)

      expect("(not)")  { SchTestConverter.evaluatePrimExpr(notExpr)         }
      expect("(=)")    { SchTestConverter.evaluatePrimExpr(eqExpr)          }
      expect("(>)")    { SchTestConverter.evaluatePrimExpr(greaterThanExpr) }
      expect("(<)")    { SchTestConverter.evaluatePrimExpr(lessThanExpr)    }
      expect("(= (+)") { SchTestConverter.evaluatePrimExpr(plusExpr)        }
      expect("(= (-)") { SchTestConverter.evaluatePrimExpr(minusExpr)       }
      expect("(= (*)") { SchTestConverter.evaluatePrimExpr(starExpr)        }
    }

    test("test term getReturnType") {
      val op1 = Operation("op1")
      val op2 = Operation("op2")
      val op3 = Operation("op3")
      val op1ret = IntLiteral(1, "1")
      val op2ret = BooleanLiteral(true, "true")
      val op3ret = TypeName("type")
      val argTypes = ArgTypes(List())
      val opSpec1 = OperationSpec(op1, argTypes, op1ret, false)
      val opSpec2 = OperationSpec(op2, argTypes, op2ret, false)
      val opSpec3 = OperationSpec(op3, argTypes, op3ret, false)
      val opSpecs = ListSet(opSpec1, opSpec2, opSpec3)

      val emptyArgs = EmptyArg()
      val term1 = TermExpr(op1, emptyArgs)
      val term2 = TermExpr(op2, emptyArgs)
      val term3 = TermExpr(op3, emptyArgs)

      expect(op1ret) { SchTestConverter.getReturnType(term1, opSpecs) }
      expect(op2ret) { SchTestConverter.getReturnType(term2, opSpecs) }
      expect(op3ret) { SchTestConverter.getReturnType(term3, opSpecs) }
    }

    test("test rhs getReturnType") {
      val op1 = Operation("op1")
      val op2 = Operation("op2")
      val op3 = Operation("op3")
      val op1ret = IntLiteral(1, "1")
      val op2ret = BooleanLiteral(true, "true")
      val op3ret = TypeName("type")
      val argTypes = ArgTypes(List())
      val opSpec1 = OperationSpec(op1, argTypes, op1ret, false)
      val opSpec2 = OperationSpec(op2, argTypes, op2ret, false)
      val opSpec3 = OperationSpec(op3, argTypes, op3ret, false)
      val opSpecs = ListSet(opSpec1, opSpec2, opSpec3)

      val emptyArgs = RhsEmptyArg()
      val term1 = RhsExpr(op1, emptyArgs)
      val term2 = RhsExpr(op2, emptyArgs)
      val term3 = RhsExpr(op3, emptyArgs)

      expect(op1ret) { SchTestConverter.getReturnType(term1, opSpecs) }
      expect(op2ret) { SchTestConverter.getReturnType(term2, opSpecs) }
      expect(op3ret) { SchTestConverter.getReturnType(term3, opSpecs) }
    }

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
  }
}
