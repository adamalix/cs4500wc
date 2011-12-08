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

    //this function is called under the pretense that the rule applies to the term
    def rewriteTerm(term: Term): Rhs = {
      term match {
        case termid: TermID => new RhsID(termIdMap(termid).toString())
        case termExpr: TermExpr => {
          val rewrittenArgs = rewriteArgs(termExpr.args)
          for (rule <- eqs) {
            if (doTermsMatch(termExpr, rewrittenArgs, rule.left)) {
              // rewrite into the rhs
            }
          }

          RhsExpr(termExpr.op, rewrittenArgs)
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
              case args: Args => doArgsMatch(rhsExpr.args, args.args)
            }
            // Match the Op of the RhsExpr to the Rule
            matchOp(rhsExpr.op, ruleArg) && argsMatch
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

    def rewriteArgs(args: Arg): RhsArg = {
      null
    }

    def ruleApplies(term: TermExpr, eq: Equation): Boolean = {
      val eqLeft: Term = eq.left
      eqLeft match {
        case leftIdent: TermID => false
        case leftExpr: TermExpr => term.op.equals(leftExpr.op) && term.args.length().equals(leftExpr.args.length())
      }
    }

/*
    def checkIfArgsEqual(termArgs: Arg, patternArgs: Arg): Boolean = {
        if (termArgs.isEmpty() && patternArgs.isEmpty())
            true
        termArgs match {
            case emptyTermArgs: EmptyArg => false
            case termArguments: Args => {
                patternArgs match {
                    case emptyPatternArgs: EmptyArg => false
                    case patternArguments: Args => {
                         termEqual(termArguments.term, patternArguments.term) &&
                         checkIfArgsEqual(termArguments.args, patternArguments.args)
                    }
                }
            }
        }
    }

    def termEqual(t1: Term, t2: Term): Boolean = {
        t1 match {
            case t1Ident:TermID => {
                t2 match {
                    case t2Ident:TermID => true
                    case t2Expr:TermExpr => false
                }
            }
            case t1Expr:TermExpr => {
                t2 match {
                    case t2Ident:TermID => false
                    case t2Expr:TermExpr => true
                }
            }
        }
    }

    def termExprEqual(t1: TermExpr, t2: TermExpr): Boolean = {
        t1.op.equals(t2.op) && t1.args.length().equals(t2.args.length())
    }
    */
  }
}
