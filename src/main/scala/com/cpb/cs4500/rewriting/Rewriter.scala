/*
 Rewrites the parsed data into Scheme tests.
 */

package com.cpb.cs4500.rewriting {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._

  class Rewriter(specification: Spec) {
    val spec = specification

    def rewrite(terms:List[Term]):List[Term] = {
        val eqs = this.spec.equations.eqs
        for (term<-terms) {
            for (eq<-eqs) {
                if (ruleApplies(term, eq)) {
                    //rule applies: rewrite term; break the loop
                    rewriteTerm(term, eq)
                }
            }
        }
        List()
    }

    //this function is called under the pretense that the rule applies to the term
    def rewriteTerm(term:Term, eq:Equation):Term = {
        term
    }

    def ruleApplies(term:Term, eq:Equation):Boolean = {
        term match {
            case term:TermID => false
            //potential ambiguity if term on left is an ID instead of an Expr 
            //case term:TermExpr => term.op.equals(eq.left.op)
        }
    }
  }
}