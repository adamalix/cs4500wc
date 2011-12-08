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
          for (rule <- eqs) {
            if (doTermsMatch(termExpr, rule.left)) {
              // rewrite into the rhs
            }
          }
          return null
          //RhsExpr(termExpr.op, rewrittenArgs)
        }
      }
    }

    // match term to a rule to see if we can rewrite
    def doTermsMatch(term: Term, rule: Term): Boolean = {
      term match {
        // we have an ID, is the rule is an ID
        case id: TermID => rule match {
          case ruleId: TermID => true
          case _ => false
        }
        // we have an expr, make sure rule is an expr
        case termExpr: TermExpr => rule match {
          // are the ops and args the same?
          case ruleExpr: TermExpr => {
            termExpr.op == ruleExpr.op &&
            doArgsMatch(termExpr.args, ruleExpr.args)
          }
          case _ => false
        }
      }
    }

    // helper for doTermsMatch to check Args in the case that
    // we have a TermExpr
    def doArgsMatch(termArg: Arg, ruleArg: Arg): Boolean = {
      termArg match {
        // termArgs is empty, is the ruleArg empty?
        case termEmpty: EmptyArg => ruleArg match {
          case ruleEmpty: EmptyArg => true
          case _ => false
        }

        // we have an Args(Term, Args)
        case termArgs: Args => ruleArg match {
          // are the term and ruleTerm the same?
          case ruleArgs: Args => {
            doTermsMatch(termArgs.term, ruleArgs.term) &&
            doArgsMatch(termArgs.args, ruleArgs.args)
          }
          case _ => false
        }
      }
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
