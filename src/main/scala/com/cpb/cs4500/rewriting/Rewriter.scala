/*
 Rewrites the parsed data into Scheme tests.
 */

package com.cpb.cs4500.rewriting {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._

  class Rewriter(specification: Spec) {
    val spec = specification
    val termIdMap = Map[TermID, TypeLiteral]()
    
    def rewrite(terms:List[Term]):List[Rhs] = {
        val eqs = this.spec.equations.eqs
        var rewrittenTerms = List[Rhs]()
        for (term<-terms) 
		{
            rewrittenTerms = rewrittenTerms :+ rewriteTerm(term)
        }  
        rewrittenTerms
    }

    //this function is called under the pretense that the rule applies to the term
    def rewriteTerm(term:Term): Rhs = {
        term match {
            case termid: TermID => {
                new RhsID(termIdMap(termid).toString())
            }
        }
    }
        
/*        for (eq<-eqs) 
			{
               term match {
                    case termid: TermID => {
                        new RH
                    }
                    case termexpr: TermExpr => {
                    }
               }
               // if (ruleApplies(term, eq)) {
                    //rule applies: rewrite term; break the loop
                //    rewriteTerm(term, eq)
               // }
            }
        term*/
    

    def ruleApplies(term: TermExpr, eq: Equation): Boolean = {
		val eqLeft:Term = eq.left
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