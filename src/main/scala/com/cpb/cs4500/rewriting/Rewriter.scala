/*
Rewrites the parsed data into Scheme tests. 
*/
package com.cpb.cs4500.rewriting {
    import com.cpb.cs4500.parsing._
    import com.cpb.cs4500.valueGeneration._
    class Rewriter() {
        def applyRewriteRules(specification:Spec):String  = {
            "HNNGG"
        }

		def rewrite(values:List[GeneratedValue], rules:List[AnyRef]):List[GeneratedValue] = {
            //for (value<-values) value match {
            //    case value:GeneratedPrimitive => rewrite(value, rules)
            //    case value:GeneratedFunction => {
            //        List(applyRules(rules, value), 
            //             rewrite(value.getArguments, rules))
            //    }
            //}
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