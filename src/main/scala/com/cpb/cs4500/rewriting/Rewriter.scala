/*
Rewrites the parsed data into Scheme tests. 
*/
package com.cpb.cs4500.rewriting {
    import com.cpb.cs4500.parsing._
    class Rewriter() {
        def applyRewriteRules(specification:Spec):String  = {
            "\n" + specification.toString
        }

       // def generateExpressions(specification:Spec):String = {
         //   specification.signatures.sigs.foreach(
       // }

       // def generateValue(symbol:Terminal) symbol match {
       //     case n: TypeName => "lol"
       //     case l: TypeLiteral => generateLiteralValue(l)
        //    case _ => "wrong again"
       // }

       // def generateLiteralValue(symbol:Terminal) = symbol.toString match {
         //   case "int" => 0
           // case "boolean" => true
           // case "char" => 'a'
           // case "string" => "hello"
           // case _ => "unknown value"
       // }
    }
}
