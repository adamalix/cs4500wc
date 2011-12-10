/*
 Rewrites the parsed data into Scheme tests.
 */

package com.cpb.cs4500.rewriting {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._
  import com.cpb.cs4500.util._

  import scala.collection.mutable.Map

  class Rewriter(specification: Spec) {
    val spec = specification
    val eqs = this.spec.equations.eqs
    val termIdMap = Map[TermID, TypeLiteral]()
    var counter = 0
    // Rewrite this list of terms into a list of Rhs
    def rewriteTerms(terms: List[Term]): List[(Term, Rhs)] = {
      var rewrittenTerms = List[(Term, Rhs)]()
      // Rewrite terms one-by-one and return this list of rewritten
      // terms
      for (term <- terms) {
        try {
          counter = 0
          rewrittenTerms = rewrittenTerms :+ (term, rewriteTerm(term))
        } catch {
          case ex: IllegalArgumentException =>
          case inf: InfiniteRewriteException => println("Rewrote infinitly for an expression")
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
        }
      }
      false
    }

    def rewriteTerm(term: Term): Rhs = {
      println("Attempting to rewrite term: " + term.toSexpr)
      term match {
        case termid: TermID => new RhsID(termid.ident)
        case termExpr: TermExpr => {
          val rewrittenArgs = rewriteArgs(termExpr.args)
          if (hasEq(termExpr)) {
            for (rule <- eqs) {
              // Deterministic, we assume that only one rule will apply
              // because we will break and return on succesful match
              println("attempting to match: " +
                      termExpr.toSexpr + " " + rewrittenArgs.toSexpr + " " + rule.left.toSexpr)
              if (doTermsMatch(termExpr, rewrittenArgs, rule.left)) {
                println("just matched: " +
                        termExpr.toSexpr + " " + rewrittenArgs.toSexpr + " " + rule.left.toSexpr)
                // rewriting magic happens here.  we find the ids
                // in the args of the rule and replace them with the
                // corresponding rewritten args
                val idMap = rule.left match {
                  case ruleExpr: TermExpr => mapIds(ruleExpr.args, rewrittenArgs, Map[TermID, Rhs]())
                  case id: TermID => throw new RuntimeException("YOU FUCKED UP AGAIN")
                }
                println("MAP: ")
                println(idMap)
                return rewriteToRhs(rule.right, idMap)
              }
            }
            // if we reach this point, we can't apply a rule and we throw
            // away the test value
            println("Throwing away: " + term.toSexpr)
            throw new IllegalArgumentException
          }
          val noRuleExpr = RhsExpr(termExpr.op, rewrittenArgs)
          return noRuleExpr
        }
      }
    }



    // Map the IDs from the left hand side of a rule (ruleArg)
    // to values inside the rewrittenArgs
    def mapIds(ruleArg: Arg, rewrittenArgs: RhsArg, idMap: Map[TermID, Rhs]): Map[TermID, Rhs] = {
      ruleArg match {
        // Args(Term, Arg)
        // check the term for pattern variables
        case ruleArgs: Args => {
          // moving left
          ruleArgs.term match {
            // we have an ID, map it!
            case termId: TermID => {
              val termRhsTuple = mapIdToRhs(termId, rewrittenArgs)
              idMap += (termRhsTuple._1 -> termRhsTuple._2)
              // now, we need to check the args of rewrittenArgs, so
              // we know that need to move right
              println("mapIds: " + ruleArgs.args.toSexpr + " " + rewrittenArgs.toSexpr + " " + idMap)
              rewrittenArgs match {
                case rwArgs: RhsArgs => mapIds(ruleArgs.args, rwArgs.args, idMap)
                /*ruleArgs.args match {
                  case ruleArgsArgs: Args => mapIds(ruleArgsArgs.args, rwArgs.args, idMap)
                  case _ => throw new RuntimeException("what were we thinking???")
                }*/
                case _ =>
                  throw new RuntimeException("we should never get Empty when moving right at this point")
              }
            }
            // ruleargs.term is a termExpr
            // we don't have an ID, so now we need to map the ruleargs.term.args against
            // rewrittenArgs.args and termArgs.args against rewrittenargs.args
            // we don't care about the ruleArgsTermExpr.op because it's not a pattern
            // variable
            case ruleArgsTermExpr: TermExpr => {
              println("going crazy " + ruleArgsTermExpr + " " + rewrittenArgs)
              rewrittenArgs match {
                case rewrittenArgsRhsArgs: RhsArgs =>
                  mapIds(ruleArgsTermExpr.args, getRhsArgsRhsArgs(rewrittenArgsRhsArgs.rhs), idMap)
                case _ => throw new RuntimeException("we might be retarded")
              }
            }
          }
        }
        // there is nothing else to add to the map!
        case ruleEmpty: EmptyArg => idMap
      }
    }

    def getTermArgsTermArgs(term: Term): Arg = {
      term match {
        case termExpr: TermExpr => termExpr.args
        case _ => throw new RuntimeException("IDUNNO")
      }
    }

    // get rhArgs.rhs.args
    def getRhsArgsRhsArgs(rhs: Rhs): RhsArg = {
      rhs match {
        case rhs: RhsExpr => rhs.args
        case rhs: RhsPrimExpr => rhs.args
        case _ => throw new RuntimeException("IDUNNO")
      }
    }

    // get the rhs of this arg and return a tuple with the termid and rhs
    def mapIdToRhs(termId: TermID, rewrittenArgs: RhsArg): (TermID, Rhs) = {
      rewrittenArgs match {
        // impossible
        case empty: RhsEmptyArg =>
          throw new RuntimeException("When getting a Rhs for TermID we shouldn't get RhsEmptyArg")
        case rhsArg: RhsArgs => (termId, rhsArg.rhs)
      }
    }

    def rewriteToRhs(rhsRule: Rhs, idMap: Map[TermID, Rhs]): Rhs = {
      if (counter < 100) {
        counter = counter + 1
        rhsRule match {
          case trueVal: RhsTrue => trueVal
          case falseVal: RhsFalse => falseVal
          case uInt: RhsUInt => uInt
          // get the value from the map and return it
          case id: RhsID => {
            return idMap(TermID(id.ident))
          }
          // recursive cases
          case rhsExpr: RhsExpr => {
            val rhsArg = resolveArgs(rhsExpr.args, idMap)
            return RhsExpr(rhsExpr.op, rhsArg)
          }
          case primExpr: RhsPrimExpr => {
            val rhsArg = resolveArgs(primExpr.args, idMap)
            return RhsPrimExpr(primExpr.prim, rhsArg)
          }
        }
      }

      else {
        throw new InfiniteRewriteException
      }
    }
    def resolveArgs(rhsArg: RhsArg, idMap: Map[TermID, Rhs]): RhsArg = {
      rhsArg match {
        case empty: RhsEmptyArg => empty
        case rhsArgs: RhsArgs => {
          val rhs = rewriteToRhs(rhsArgs.rhs, idMap)
          val args = resolveArgs(rhsArgs.args, idMap)
          RhsArgs(rhs, args)
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
        // We have a TermID, we must match if it isn't!
        // empty!
        case ruleId: TermID => !rewrittenArgs.isEmpty
      }
    }

    // compare rewritten args to the rule arguments
    def doArgsMatch(rhsArg: RhsArg, ruleArg: Arg): Boolean = {
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
      arg match {
        case empty: EmptyArg => false
        case args: Args => args.term match {
          case id: TermID => false
          case expr: TermExpr => op.equals(expr.op)
        }
      }
    }

    def rewriteArgs(arg: Arg): RhsArg = {
      arg match {
        case empty: EmptyArg => RhsEmptyArg()
        case args: Args => {
          val rewrittenArgs = rewriteArgs(args.args)
          RhsArgs(rewriteTerm(args.term), rewrittenArgs)
        }
      }
    }

  }

  object Rewriter {
    def main(args: Array[String]): Unit = {
      try {
      import com.cpb.cs4500.parsing.ADTParser ;
      import com.cpb.cs4500.parsing.Spec ;
      import com.cpb.cs4500.io.ReadWriter ;
      import com.cpb.cs4500.valueGeneration.ValueGenerator ;
      val testFileName3 = "src/test/resources/test3" ;
      val testFile3 = ReadWriter.inputFromFile(testFileName3) ;
      val parser = new ADTParser() ;
      val spec3 = parser.parseAll(parser.spec, testFile3).get ;
      val gen = new ValueGenerator(spec3) ;
      val rewriter = new Rewriter(spec3) ;
      val terms1 = gen.createAllTests(3);
      val rewrittenTuples1 = rewriter.rewriteTerms(terms1) ;
      for ((left, right) <- rewrittenTuples1) { println(left); println(right) }
      } catch {
        case ex: RuntimeException =>
      }
    }
  }

}
