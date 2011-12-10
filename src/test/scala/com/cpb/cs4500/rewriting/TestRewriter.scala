package com.cpb.cs4500.rewriting {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.rewriting._
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration.ValueGenerator
  import com.cpb.cs4500.io.ReadWriter

  class TestRewriter extends FunSuite {

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
    - rewriteTerms
    - rewriteTerm
    - doTermsMatch
    - doArgsMatch
    x matchOp
    - rewriteArgs
    */

    test("test doArgsMatch") {
      val spec5 = parser.parseAll(parser.spec, testFile7).get
      val rewriter = new Rewriter(spec5)

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
    }

    test("test matchOp") {
      val spec5 = parser.parseAll(parser.spec, testFile5).get
      val rewriter = new Rewriter(spec5)

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

    test("teset doTermsMatch") {
      val spec3 = parser.parseAll(parser.spec, testFile3).get
      val rewriter = new Rewriter(spec3)

      val popOp = Operation("pop")
      val emptyOp = Operation("empty")
      val pushOp = Operation("push")
      val topOp = Operation("top")
      val emptyArg = EmptyArg()
      val emptyExpr = TermExpr(emptyOp, emptyArg)
      val emptyExprAsArgs = Args(emptyExpr, emptyArg)
      val rewrittenEmptyExpr = rewriter.rewriteArgs(emptyExprAsArgs)
      // TermExpr(pop, Args(TermExpr(empty, EmptyArg()), EmptyArg()))
      val term1 = TermExpr(popOp, emptyExprAsArgs)

      val pushArgs = Args(TermID("s"), Args(TermID("k"), EmptyArg()))
      val pushSKExpr = TermExpr(pushOp, pushArgs)
      val pushSKExprArgs = Args(pushSKExpr, EmptyArg())
      val rewrittenPushSKExprArgs = rewriter.rewriteArgs(pushSKExprArgs)
      val topPushSKExpr = TermExpr(topOp, pushSKExprArgs)

      /*println(pushSKExpr)
      println(pushSKExprArgs)
      println(rewrittenPushSKExprArgs)
      println(topPushSKExpr)*/

      // (pop (push s k)) = s
      val popPushRule = spec3.equations.eqs(1)
      // (top (push s k)) = k
      val topPushRule = spec3.equations.eqs(0)
      /*
      println("PopPushRule: ")
      println(popPushRule)
      println("TopPushRule: ")
      println(topPushRule)

      println("********** STARTING TEST OUTPUT **********")*/

      expect(false) {
        rewriter.doTermsMatch(term1, rewrittenEmptyExpr, topPushRule.left)
      }

      expect(false) {
        rewriter.doTermsMatch(term1, rewrittenEmptyExpr, popPushRule.left)
      }

      expect(true) {
        rewriter.doTermsMatch(topPushSKExpr, rewrittenPushSKExprArgs, topPushRule.left)
      }

    }

    test("test hasEq") {
      val spec3 = parser.parseAll(parser.spec, testFile3).get
      val rewriter = new Rewriter(spec3)
      val emptyOp = Operation("empty")
      val emptyArg = EmptyArg()
      val emptyExpr = TermExpr(emptyOp, emptyArg)

      val popOp = Operation("pop")
      val emptyExprAsArgs = Args(emptyExpr, emptyArg)
      val term1 = TermExpr(popOp, emptyExprAsArgs)
      expect(false) {
        rewriter.hasEq(emptyExpr)
      }

      expect(true) {
        rewriter.hasEq(term1)
      }
    }

    test("test MapIds") {
      val spec3 = parser.parseAll(parser.spec, testFile3).get
      val rewriter = new Rewriter(spec3)

      //ruleArg = (push s k)
      val pushOp = Operation("push")
      val emptyArgs = EmptyArg()
      val sID = TermID("s")
      val kID = TermID("k")
      val mID = TermID("m")
      val zID = TermID("z")
      
      val pushArgs = Args(sID, Args(kID, Args(mID, Args(zID, emptyArgs))))
      val pushExpr = TermExpr(pushOp, pushArgs)
      val ruleArg = Args(pushExpr, emptyArgs)

      //rewrittenArgs = (push (empty) 1)
      val emptyOp = Operation("empty")
      val rhsEmptyArgs = RhsEmptyArg()
      val emptyExpr = RhsExpr(emptyOp, rhsEmptyArgs)
      val one = RhsUInt("1")
      val two = RhsUInt("2")
      val three = RhsUInt("3")
      val argsArgs = RhsArgs(emptyExpr, RhsArgs(one, RhsArgs(two, RhsArgs(three, rhsEmptyArgs))))

      //expectedMap = Map(s -> (empty), k -> 1)
      val expectedMap = Map(sID -> emptyExpr, kID -> one, mID -> two, zID -> three)
      val inputMap = scala.collection.mutable.Map[TermID, Rhs]()

      expect(expectedMap) { rewriter.mapIds(pushArgs, argsArgs, inputMap) }
      
    }

    test("test rewriteTerm") {

      val spec3 = parser.parseAll(parser.spec, testFile7).get
      val gen = new ValueGenerator(spec3)
      val rewriter = new Rewriter(spec3)
      //ruleArg = (push s k)
      val pushOp = Operation("push")
      val emptyArgs = EmptyArg()
      val sID = TermID("s")
      val kID = TermID("k")
      val pushArgs = Args(sID, Args(kID, emptyArgs))
      val pushExpr = TermExpr(pushOp, pushArgs)
      val ruleArg = Args(pushExpr, emptyArgs)
      
      val createdTests = gen.createAllTests(2);
      for (t <- createdTests) {
        println(t.toSexpr)
      }

      //rewrittenArgs = (push (empty) 1)
      val emptyOp = Operation("empty")
      val rhsEmptyArgs = RhsEmptyArg()
      val emptyExpr = RhsExpr(emptyOp, rhsEmptyArgs)
      val one = RhsUInt("1")
      val argsArgs = RhsArgs(emptyExpr, RhsArgs(one, rhsEmptyArgs))

      val something = rewriter.rewriteTerm(pushExpr)
      println("rewriteterm running: ")
      println(pushExpr)
      println(something)

      val bullshit = TermExpr(pushOp,Args(TermExpr(emptyOp,EmptyArg()),Args(TermID("8"),EmptyArg())))

      expect(true) {
        try {
          val bullstuff = rewriter.rewriteTerm(bullshit)
          println("BULLSTUFF: ")
          println(bullshit)
          println(bullstuff)
          bullstuff
        } catch {
          case ex: RuntimeException => false
        }
      }


    }


  }
}
