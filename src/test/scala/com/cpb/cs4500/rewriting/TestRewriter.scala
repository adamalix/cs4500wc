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

    test("test doArgsMatch"){
      //need rhsArg and Arg
      val emptyArg = EmptyArg()
      val rhsEmptyArg = RhsEmptyArg()
      val op1 = Operation("operation1")
      val testTermExpr = TermExpr(op1, emptyArg)
      val testArgs1 = Args(testTermExpr, emptyArg)
      val testRhsExpr = RhsExpr(op1, rhsEmptyArg)
      val testRhsArgs1 = RhsArgs(testRhsExpr, rhsEmptyArg)

      expect(true)  { rewriter.doArgsMatch(rhsEmptyArg,  emptyArg)  }
      expect(false) { rewriter.doArgsMatch(testRhsArgs1, emptyArg)  }
      expect(false) { rewriter.doArgsMatch(rhsEmptyArg,  testArgs1) }
      expect(true)  { rewriter.doArgsMatch(testRhsArgs1, testArgs1) }
    }

    test("test matchOp"){
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
