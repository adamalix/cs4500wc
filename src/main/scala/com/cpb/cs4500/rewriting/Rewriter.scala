/*
 Rewrites the parsed data into Scheme tests.
 */

package com.cpb.cs4500.rewriting {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._

  class Rewriter(specification: Spec) {
    val spec = specification
    def applyRewriteRules(specification:Spec):String  = {
      "HNNGG"
    }

    def rewriteTerm(term: Term) = {
      null
    }

    def rewrite(values:List[GeneratedValue], rules:List[AnyRef]):List[GeneratedValue] = {
      List()
    }

    def applyRules(rules:List[AnyRef], value:GeneratedValue):GeneratedValue = {
      val ruleApplies = true
      for (rule<-rules) {
        if (ruleApplies) {
          //rewrite the GeneratedValue, probably by making a new one
          value
        }
      }
      value
    }
  }
}
