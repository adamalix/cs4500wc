/**
 * Class used to convert tuples of left and rhs to a test Sexpr
 */

package com.cpb.cs4500.util {
  import com.cpb.cs4500.parsing._

  import scala.collection.immutable.ListSet

  object SchTestConverter {

    // converts the term and it's rewritten counterpart into a test expression
    def createTestSexpr(pair: Tuple2[Term, Rhs], count: Int, opspecs: ListSet[OperationSpec]): String = {
      println("generating test for: " + pair)
      try {
        tupleToTest(pair, opspecs, count)
      } catch {
        case rt: RuntimeException => ""
      }
    }

    def tupleToTest(pair: Tuple2[Term, Rhs], opspecs: ListSet[OperationSpec], count: Int): String = {
      val start = "(test \"test" + count + getOpName(pair._1) + "\" "
      val leftReturnType = getReturnType(pair._1, opspecs)
      val eqRightSide = pair._2 match {
        // need to fix this change comparator on return type
        case rhsExpr: RhsExpr => {
          val wrappedExpr = wrapRhsExpr(rhsExpr, opspecs, leftReturnType)
          leftReturnType match {
            case integer: IntLiteral =>
              "(= " + wrappedExpr
            case char: CharLiteral =>
              "(char=? " + wrappedExpr
            case str: StringLiteral =>
              "(string=? " + wrappedExpr
            case bool: BooleanLiteral =>
              "(boolean=? " + wrappedExpr
            case other: TypeName =>
              wrappedExpr
          }
        }
        case primExpr: RhsPrimExpr => evaluatePrimExpr(primExpr)
        case int: RhsUInt => "(= " + int.toSexpr
        case rhsTrue: RhsTrue => "("
        case rhsFalse: RhsFalse => "(not "
        // this might be the case for strings or chars, shit.
        // might need to make a method to determine whether or not
        // it's length one in which case it'd be a char
        // this needs to check type
        case rhsID: RhsID => "(=? " + rhsID.toSexpr
      }

      val eqLeftSide = pair._1 match {
        case id: TermID => throw new RuntimeException("can't have an ID here")
        case term: TermExpr => wrapTerm(term, opspecs, leftReturnType)
      }

      return start + eqRightSide + " " + eqLeftSide + ")\n"
    }


    // TODO: write this function...
    def evaluatePrimExpr(expr: RhsPrimExpr): String = {
      expr.prim match {
        case not: Not => return expr.toSexpr
        case plus: Plus => return "(= " +  expr.toSexpr
        case minus: Minus => return "(= " + expr.toSexpr
        case star: Star => return "(= " + expr.toSexpr
        case eq: Eq => return expr.toSexpr
        case greater: GreaterThan => return expr.toSexpr
        case less: LessThan => return expr.toSexpr
      }
    }

    def wrapRhsExpr(expr: RhsExpr, opspecs: ListSet[OperationSpec], targetReturnType: Terminal): String = {
      // need to get return type based on op.
      // getReturnType(expr)
      // if it's a primitive, great! it doesn't need to be wrapped
      // if it's NOT, we need to find the first opSpec that takes the return
      //   type of that expr, only one argument, and that RETURNS a primitive

      val returnType = getReturnType(expr, opspecs)

      returnType match {
        case typeLit: TypeLiteral => return expr.toSexpr
        case typeName: TypeName => return findRhsWrapper(expr, targetReturnType, typeName, opspecs)
      }

    }

    def getReturnType(expr: Term, opspecs: ListSet[OperationSpec]): Terminal = {
      expr match {
        case termExpr: TermExpr => {
          val op = termExpr.op

          for (opspec <- opspecs) {
            if (termExpr.op == opspec.op) {
              return opspec.returnType
            }
          }

          throw new RuntimeException("Couldn't find that op, must be a mistake.")
        }
        case termID: TermID => throw new RuntimeException("Can't have an ID here.")
      }
    }


    def getReturnType(expr: Rhs, opspecs: ListSet[OperationSpec]): Terminal = {
      expr match {
        case rhsExpr: RhsExpr => {
          val op = rhsExpr.op

          for (opspec <- opspecs) {
            if (rhsExpr.op == opspec.op) {
              return opspec.returnType
            }
          }

          throw new RuntimeException("Couldn't find that op, must be a mistake.")
        }
        case rhsID: RhsID => throw new RuntimeException("Can't have an ID here.")
      }
    }

    def wrapTerm(term: TermExpr, opspecs: ListSet[OperationSpec], targetReturnType: Terminal): String = {
      val returnType = getReturnType(term, opspecs)

      returnType match {
        case typeLit: TypeLiteral => return term.toSexpr
        case typeName: TypeName => return findTermWrapper(term, targetReturnType, typeName, opspecs) 
      }
    }

    def findRhsWrapper(expr: RhsExpr, returnType: Terminal, typeName: TypeName, opSpecs: ListSet[OperationSpec]): String = {
      // look through opspecs for one that takes the type name and returns
      // the return type
      for (opSpec <- opSpecs) {
        opSpec.returnType match {
          case lit: TypeLiteral => {
            val opArgs = opSpec.getArgTypes
            if (opArgs.length == 1 && opArgs.contains(typeName) && lit == returnType) {
              println("finding wrapper for: " + opSpec.op)
              return "(" + opSpec.op + " " + expr.toSexpr + ")"
            }
          }
          case _ =>
        }
      }
      // TODO: cant find a wrapper so we should throw the test away
      throw new RuntimeException("Can't find a wrapper.")
    }

    def findTermWrapper(expr: TermExpr, returnType: Terminal, typeName: TypeName, opSpecs: ListSet[OperationSpec]): String = {
      // look through opspecs for one that takes the type name and returns
      // the return type
      for (opSpec <- opSpecs) {
        opSpec.returnType match {
          case lit: TypeLiteral => {
            val opArgs = opSpec.getArgTypes
            if (opArgs.length == 1 && opArgs.contains(typeName) && lit == returnType) {
              println("finding wrapper for: " + opSpec.op)
              return "(" + opSpec.op + " " + expr.toSexpr + ")"
            }
          }
          case _ =>
        }
      }
      // TODO: cant find a wrapper so we should throw the test away
      throw new RuntimeException("Can't find a wrapper.")
    }



/*
    def tupleToTest(pair: Tuple2[Term, Rhs], opspecs: ListSet[OperationSpec]): String = {
      // we need to know if we need to wrap the values or just return
      // their sexprs. this depends on the rhs value return type
      val eqRightSide = pair._2 match {
        case rhsFalse: RhsFalse => return "(not " + pair._1.toSexpr
        case rhsTrue: RhsTrue => return pair._1.toSexpr
        case int: RhsUInt => "(= " + int.toSexpr
        // Interesting cases
        case rhsExpr: RhsExpr => findWrapperForOp(rhsExpr.op, opspecs)
        case primExpr: RhsPrimExpr => primExpr.prim match {
          // All the boolean primExprs dont' need to be compared,
          // just evaluated
          case gt: GreaterThan => return pair._2.toSexpr
          case lt: LessThan => return pair._2.toSexpr
          case eq: Eq => return pair._2.toSexpr
          case not: Not => return pair._2.toSexpr
          // default to wrapping in "(= " because these compre scheme
          // int values
          case star: Star => "(= " + pair._2.toSexpr
          case minus: Minus => "(= " + pair._2.toSexpr
          case plus: Plus => "(= " + pair._2.toSexpr
        }
        case _ => throw new RuntimeException("we shouldn't have IDs here")
      }
      // hack
      if (eqRightSide == "bool")
        return pair._2.toSexpr

      val leftSide = pair._1 match {
        case expr: TermExpr => {
          findWrapperForOp(expr.op, opspecs)
        }
        case id: TermID => throw new RuntimeException("broken tuple")
      }

      eqRightSide + " " + leftSide + ")"
    }

    def findWrapperForOp(op: Operation, opSpecs: ListSet[OperationSpec]): String = {
      // need to get opSpec for op to find return type
      // then find opSpec with that return type and wrap it
      for (opSpec <- opSpecs) {
        if (op == opSpec.op) {
          val retType = opSpec.returnType
          return findSchConverter(retType, opSpecs)
        }
      }
      throw new RuntimeException("we couldn't convert to a scheme value")
    }

    def findSchConverter(returnType: Terminal, opSpecs: ListSet[OperationSpec]): String = {
      println("matching returntype: " + returnType)
      returnType match {
        case lit: TypeLiteral => lit match {
          case num: IntLiteral => return "(= "
          // hack
          case bool: BooleanLiteral => return "bool"
          case char: CharLiteral => return "(char=? "
          case str: StringLiteral => return "(string=? "
        }
        // we need to check if there is an opSpec whose returnType is
        // a TypeLiteral, has an ags length of 1, and argument is
        // the returnType that we want
        case typeName: TypeName => {
          println("got a typename: " + typeName)
          for (opSpec <- opSpecs) {
            opSpec.returnType match {
              case lit: TypeLiteral => {
                val opArgs = opSpec.getArgTypes
                if (opArgs.length == 1 && opArgs.contains(typeName)) {
                  println("finding wrapper for: " + opSpec.op)
                  return "(" + opSpec.op + " "
                }
              }
              case _ =>
            }
          }
        }
      }
      throw new RuntimeException("No OpSpec returns this type, Impossible.")
    }

*/
    def getOpName(term: Term): String = {
      term match {
        case termExpr: TermExpr => termExpr.op.toString
        case termID: TermID => ""
      }
    }

  }
}
