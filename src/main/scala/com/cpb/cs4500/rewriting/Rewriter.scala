package com.cpb.cs4500.rewriting {
    import com.cpb.cs4500.parsing._
    class Rewriter() {
        def applyRewriteRules(specification:Spec):String  = {
            "\n" + specification.toString
        }
    }
}