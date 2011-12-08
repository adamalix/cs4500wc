package com.cpb.cs4500.rewriting {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.rewriting._
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.io._

  class TestRewriter extends FunSuite {

    val parser = new ADTParser()
    val testFileName = "src/test/resources/test5"
    val testFile = ReadWriter.inputFromFile(testFileName)
    val spec = parser.parseAll(parser.spec, testFile).get
    val rewriter = new Rewriter(spec)


    /*
    Functions to potentially test:
    - rewriteTerms
    - rewriteTerm
    - doTermsMatch
    - doArgsMatch
    x matchOp
    - rewriteArgs
    */

    test("test doArgsMatch") {
      //need rhsArg and Arg
      val emptyArg = EmptyArg()
      val rhsEmptyArg = RhsEmptyArg()
      val op1 = Operation("operation1")
      val op2 = Operation("operation2")

      // TermExpr(Operation("op1"), EmptyArg()
      val testTermExpr = TermExpr(op1, emptyArg)
      // Args(TermExpr(Operation("op1"), EmptyArg()), EmptyArg())
      val testArgs1 = Args(testTermExpr, emptyArg)
      // TermExpr(Operation("op2"), Args(TermExpr(Operation("op1"), EmptyArg()), EmptyArg())
      val testTermExpr2 = TermExpr(op2, testArgs1)
      val testArgs2 = Args(testTermExpr2, testArgs1)

      val testRhsExpr1 = RhsExpr(op1, rhsEmptyArg)
      val testRhsArgs1 = RhsArgs(testRhsExpr1, rhsEmptyArg)
      val testRhsExpr2 = RhsExpr(op2, testRhsArgs1)
      val testRhsArgs2 = RhsArgs(testRhsExpr2, testRhsArgs1)


      expect(true)  { rewriter.doArgsMatch(rhsEmptyArg,  emptyArg)  }
      expect(false) { rewriter.doArgsMatch(testRhsArgs1, emptyArg)  }
      expect(false) { rewriter.doArgsMatch(rhsEmptyArg,  testArgs1) }
      expect(true)  { rewriter.doArgsMatch(testRhsArgs1, testArgs1) }
      expect(true)  { rewriter.doArgsMatch(testRhsArgs2, testArgs2) }
      //expect(true)  { rewriter.doTermsMatch(testTermExpr2, 
    }

    test("test matchOp") {
      val op1 = Operation("operation1")
      val op2 = Operation("operation2")
      val emptyArg = EmptyArg()
      val testTermID = TermID("identifier")
      val testTermExpr = TermExpr(op1, emptyArg)
      val args1 = Args(testTermExpr, emptyArg)
      val args2 = Args(testTermID, args1)

      expect(false) { rewriter.matchOp(op1, emptyArg) }
      expect(true)  { rewriter.matchOp(op1, args1) }
      expect(false) { rewriter.matchOp(op2, args1) }
      expect(false) { rewriter.matchOp(op1, args2) }
    }



  }
}
