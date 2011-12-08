/*
 Rewrites the parsed data into Scheme tests.
 */

package com.cpb.cs4500.rewriting {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._

  class Rewriter(specification: Spec) {
    val spec = specification
    val eqs = this.spec.equations.eqs
    val termIdMap = Map[TermID, TypeLiteral]()

    // Rewrite this list of terms into a list of Rhs
    def rewriteTerms(terms: List[Term]): List[Rhs] = {
      var rewrittenTerms = List[Rhs]()
      // Rewrite terms one-by-one and return this list of rewritten
      // terms
      for (term <- terms) {
        rewrittenTerms = rewrittenTerms :+ rewriteTerm(term)
      }
      rewrittenTerms
    }

    def rewriteTerm(term: Term): Rhs = {
      term match {
        case termid: TermID => new RhsID(termIdMap(termid).toString())
        case termExpr: TermExpr => {
          val rewrittenArgs = rewriteArgs(termExpr.args)
          for (rule <- eqs) {
            // Deterministic, we assume that only one rule will apply
            // because we will break and return on succesful match
            if (doTermsMatch(termExpr, rewrittenArgs, rule.left)) {
              // rewriting magic happens here

              

              return RhsExpr(termExpr.op, rewrittenArgs)
            }
          }
          // if we reach this point, we can't apply a rule and we throw
          // away the test value
          null
        }
      }
    }

    // match term to a rule to see if we can rewrite
    def doTermsMatch(termExpr: TermExpr, rewrittenArgs: RhsArg, rule: Term): Boolean = {
      rule match {
        // are the ops and args the same?
        case ruleExpr: TermExpr => {
          termExpr.op == ruleExpr.op &&
          doArgsMatch(rewrittenArgs, ruleExpr.args)
        }
        case _ => false
      }
    }

    // compare rewritten args to the rule arguments
    def doArgsMatch(rhsArg: RhsArg, ruleArg: Arg): Boolean = {
      rhsArg match {
        // rhsArg is empty, is the ruleArg empty?
        case rhsEmpty: RhsEmptyArg => ruleArg.isEmpty
        // we have an RhsArgs(Rhs, RhsArgs) and we need to
        // determine if the Rhs is an Expr so we can match
        // against the ruleArg since it isn't empty
        case rhsArgs: RhsArgs => rhsArgs match {
          case rhsExpr: RhsExpr => {
            // match against the rule and determine if its
            // args match the args of our RhsExpr
            val argsMatch = ruleArg match {
              case empty: EmptyArg => false
              case ruleArgs: Args => doArgsMatch(rhsExpr.args, ruleArgs.args)
            }
            // Match the Op of the RhsExpr to the Rule
            argsMatch && matchOp(rhsExpr.op, ruleArg)
          }
          case _ => false
        }
      }
    }

    def matchOp(op: Operation, arg: Arg): Boolean = {
      arg match {
        case empty: EmptyArg => false
        case args: Args => args.term match {
          case id: TermID => false
          case expr: TermExpr => op == expr.op
        }
      }
    }

    def rewriteArgs(arg: Arg): RhsArg = {
      arg match {
        case empty: EmptyArg => RhsEmptyArg()
        case args: Args => {
          RhsArgs(rewriteTerm(args.term), rewriteArgs(args.args))
        }
      }
    }

  }
}
