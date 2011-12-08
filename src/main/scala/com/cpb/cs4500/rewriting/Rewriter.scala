/*
 Rewrites the parsed data into Scheme tests.
 */

package com.cpb.cs4500.rewriting {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._

  import scala.collection.mutable.Map

  class Rewriter(specification: Spec) {
    val spec = specification
    val eqs = this.spec.equations.eqs
    val termIdMap = Map[TermID, TypeLiteral]()

    // Rewrite this list of terms into a list of Rhs
    def rewriteTerms(terms: List[Term]): List[(Term, Rhs)] = {
      var rewrittenTerms = List[(Term, Rhs)]()
      // Rewrite terms one-by-one and return this list of rewritten
      // terms
      for (term <- terms) {
        try {
          rewrittenTerms = rewrittenTerms :+ (term, rewriteTerm(term))
        } catch {
          case ex: IllegalArgumentException =>
        }
      }
      rewrittenTerms
    }

    def hasEq(term: TermExpr): Boolean = {
      for (rule <- eqs) {
        rule.left match {
          case ruleExpr: TermExpr => {
            if (ruleExpr.op == term.op)
              return true
          }
          case _ =>
        }
      }
      false
    }

    def rewriteTerm(term: Term): Rhs = {
      term match {
        case termid: TermID => new RhsID(termid.ident)
        case termExpr: TermExpr => {
          val rewrittenArgs = rewriteArgs(termExpr.args)
          if (hasEq(termExpr)) {
            for (rule <- eqs) {
              // Deterministic, we assume that only one rule will apply
              // because we will break and return on succesful match
              /*println("testing to see if these terms match: ")
              println("Term: " + termExpr)
              println("Rewritten Args: " + rewrittenArgs)
              println("Left Rule: " + rule.left)*/
              if (doTermsMatch(termExpr, rewrittenArgs, rule.left)) {
                // rewriting magic happens here.  we find the ids
                // in the args of the rule and replace them with the
                // corresponding rewritten args
                val idMap = mapIds(termExpr.args, rewrittenArgs, Map[TermID, Rhs]())
                println("ID Map: ")
                println(idMap)
                rewriteToRhs(rule.right, idMap)
              }
            }
            // if we reach this point, we can't apply a rule and we throw
            // away the test value
            println("No rule applies")
            throw new IllegalArgumentException
          }
          val shitty = RhsExpr(termExpr.op, rewrittenArgs)
          /*println("Rule doesn't exist for: " + termExpr.op + ", on: " + rewrittenArgs)
          println(shitty)*/
          return shitty
        }
      }
    }

    def mapIds(ruleArg: Arg, rewrittenArgs: RhsArg, idMap: Map[TermID, Rhs]): Map[TermID, Rhs] = {
      ruleArg match {
        // we have run out of pattern variables, return the map
        case empty: EmptyArg => idMap
        // we may have more pattern variables, look at the args of the rule
        case ruleArgs: Args => {
          ruleArgs.term match {
            // we have a pattern variable, put it in the map
            case id: TermID => {
              // mutate the map to contain the id mapped to the rhs
              rewrittenArgs match {
                case rhsArgs: RhsArgs => {
                  idMap += (id -> rhsArgs.rhs)
                  println("ID Map inside mapIds: " + idMap)
                  mapIds(ruleArgs.args, rhsArgs.args, idMap)
                }
                case _ => throw new RuntimeException("WTF IS GOING ON HERE")
              }
            }
            // we don't have a pattern variable, recur and look for more until
            // we have empty ruleArgs
            case _ => {
              rewrittenArgs match {
                case rhsArgs: RhsArgs => {
                  // nothing to put in the map
                  mapIds(ruleArgs.args, rhsArgs.args, idMap)
                }
                case _ => throw new RuntimeException("WTF IS GOING ON HERE")
              }
            }
          }
        }
      }
    }

    def rewriteToRhs(rhsRule: Rhs, idMap: Map[TermID, Rhs]): Rhs = {
      rhsRule match {
        case trueVal: RhsTrue => trueVal
        case falseVal: RhsFalse => falseVal
        case uInt: RhsUInt => uInt
        // get the value from the map and return it
        case id: RhsID => idMap(TermID(id.ident))
        // recursive cases
        case rhsExpr: RhsExpr => {
          val rhsArg = resolveArgs(rhsExpr.args, idMap)
          RhsExpr(rhsExpr.op, rhsArg)
        }
        case primExpr: RhsPrimExpr => {
          val rhsArg = resolveArgs(primExpr.args, idMap)
          RhsPrimExpr(primExpr.prim, rhsArg)
        }

      }
    }

    def resolveArgs(rhsArg: RhsArg, idMap: Map[TermID, Rhs]): RhsArg = {
      rhsArg match {
        case empty: RhsEmptyArg => empty
        case rhsArgs: RhsArgs => {
          val rhs = rewriteToRhs(rhsArgs.rhs, idMap)
          val args = resolveArgs(rhsArgs, idMap)
          RhsArgs(rhs, args)
        }
      }
    }

    // match term to a rule to see if we can rewrite
    def doTermsMatch(termExpr: TermExpr, rewrittenArgs: RhsArg, rule: Term): Boolean = {
      /*println("DoTermsMatch called on:")
      println("TermExpr: " + termExpr)
      println("RhsArg: " + rewrittenArgs)
      println("With rule: " + rule)*/
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
      /*println("DoArgsMatch called on:")
      println("RhsArg: " + rhsArg)
      println("RuleArg: " + ruleArg)*/
      rhsArg match {
        // rhsArg is empty, is the ruleArg empty?
        case rhsEmpty: RhsEmptyArg => ruleArg.isEmpty
        // we have an RhsArgs(Rhs, RhsArg) and we need to
        // determine if the Rhs is an Expr so we can match
        // against the ruleArg since it isn't empty
        case rhsArgs: RhsArgs => rhsArgs.rhs match {
          case rhsExpr: RhsExpr => {
            // match against the rule and determine if its
            // args match the args of our RhsExpr
            val argsMatch = ruleArg match {
              case empty: EmptyArg => false
              // now we need to make sure the rest of the args in the rhsArgs
              // have the same op types, so match that shit!
              case ruleArgs: Args => doArgsMatch(rhsArgs.args, ruleArgs.args)
            }
            // Match the Op of the RhsExpr to the Rule
            argsMatch && matchOp(rhsExpr.op, ruleArg)
          }
          case _ => false
        }
      }
    }

    def matchOp(op: Operation, arg: Arg): Boolean = {
      /*println("MatchOp called on: ")
      println("Operation: " + op)
      println("Arg: " + arg)*/
      arg match {
        case empty: EmptyArg => false
        case args: Args => args.term match {
          case id: TermID => false
          case expr: TermExpr => op.equals(expr.op)
        }
      }
    }

    def rewriteArgs(arg: Arg): RhsArg = {
      //println("Rewriting args: " + arg)
      arg match {
        case empty: EmptyArg => RhsEmptyArg()
        case args: Args => {
          val rewrittenArgs = rewriteArgs(args.args)
          RhsArgs(rewriteTerm(args.term), rewrittenArgs)
        }
      }
    }

  }
}
