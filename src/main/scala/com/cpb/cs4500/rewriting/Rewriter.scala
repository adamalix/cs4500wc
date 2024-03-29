/**
 * Rewrites the parsed data into Scheme tests.
 * Entry point: rewriteTerms(terms: List[Term])
 */

package com.cpb.cs4500.rewriting {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._
  import com.cpb.cs4500.util.InfiniteRewriteException

  import scala.collection.mutable.Map

  class Rewriter(specification: Spec) {
    val spec = specification
    val eqs = this.spec.equations.eqs

    // infinite recursion counter
    var counter = 0

    // Rewrite this list of terms into a list of Rhs
    def rewriteTerms(terms: List[Term]): List[(Term, Rhs)] = {
      var rewrittenTerms = List[(Term, Rhs)]()
      // Rewrite terms one-by-one and return a tuple of term -> rewritten terms
      for (term <- terms) {
        try {
          // reset infinite recursion counter
          counter = 0
          rewrittenTerms = rewrittenTerms :+ (term, rewriteTerm(term))
        } catch {
          // We have a bad term, do nothing and continue
          case ex: IllegalArgumentException =>
          // We began to rewrite in what looked like an infinite fashion,
          // do nothing and continue
          case inf: InfiniteRewriteException =>
        }
      }
      rewrittenTerms
    }

    // Check to see if this term has an equation in the spec
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

    // Rewrite an individual term
    def rewriteTerm(term: Term): Rhs = {
      term match {
        case termid: TermID => new RhsID(termid.ident)
        case termExpr: TermExpr => {
          val rewrittenArgs = rewriteArgs(termExpr.args)
          if (hasEq(termExpr)) {
            for (rule <- eqs) {
              // Deterministic, we assume that only one rule will apply
              // because we will break and return on succesful match
              if (doTermsMatch(termExpr, rewrittenArgs, rule.left)) {
                // rewriting magic happens here.  we find the ids
                // in the args of the rule and replace them with the
                // corresponding rewritten args
                val idMap = rule.left match {
                  case ruleExpr: TermExpr =>
                    mapIds(ruleExpr.args, rewrittenArgs, Map[TermID, Rhs]())

                  case id: TermID =>
                    throw new RuntimeException("Given an invalid statment")
                }
                return rewriteToRhs(rule.right, idMap)
              }
            }
            // if we reach this point, we can't apply a rule and we throw
            // away the test value
            throw new IllegalArgumentException
          }
          // This has no rewrite rule, return a RhsExpr with the original Op
          val noRuleExpr = RhsExpr(termExpr.op, rewrittenArgs)
          return noRuleExpr
        }
      }
    }



    // Map the IDs from the left hand side of a rule (ruleArg)
    // to values inside the rewrittenArgs
    // mutates the input map and then returns it.
    // TODO: Make this less retarded, AKA stop mutating and then returning, return a new map
    // each time.  This will need to be a change to mapIds and have a
    // var map = scala.collection.immutable.HashMap
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
              rewrittenArgs match {
                case rwArgs: RhsArgs =>
                  mapIds(ruleArgs.args, rwArgs.args, idMap)

                case _ =>
                  throw new RuntimeException("we should never get Empty when moving right at this point")
              }
            }
            // ruleargs.term is a termExpr
            // we don't have an ID, so now we need to map the ruleargs.term.args against
            // rewrittenArgs.args and ruleArgs.args against rewrittenargs.args
            // we don't care about the ruleArgsTermExpr.op because it's not a pattern
            // variable
            case ruleArgsTermExpr: TermExpr => {
              rewrittenArgs match {
                case rewrittenArgsRhsArgs: RhsArgs => {
                  mapIds(ruleArgsTermExpr.args, getRhsArgsRhsArgs(rewrittenArgsRhsArgs.rhs), idMap)
                  mapIds(ruleArgs.args, rewrittenArgsRhsArgs.args, idMap)
                }
                case _ => throw new RuntimeException("Improper rewrittenArgs")
              }
            }
          }
        }
        // there is nothing else to add to the map!
        case ruleEmpty: EmptyArg => idMap
      }
    }

    // shorthand so we don't need to nest so many match statements
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

    // reduce this rhs to its rhs form
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
      else
        throw new InfiniteRewriteException
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
}
