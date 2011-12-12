/**
 * Class used to convert tuples of left and rhs to a test Sexpr
 */

package com.cpb.cs4500.util {
  import com.cpb.cs4500.parsing._

  import scala.collection.immutable.ListSet

  object SchTestConverter {

    // converts the term and it's rewritten counterpart into a test expression
    def createTestSexpr(pair: Tuple2[Term, Rhs], count: Int, opspecs: ListSet[OperationSpec]): String = {
      try {
        tupleToTest(pair, opspecs, count)
      } catch {
        case rt: RuntimeException => ""
      }
    }

    // takes a tuple and a listset of opspecs and finds converts the tuple into
    // a valid scheme test
    def tupleToTest(pair: Tuple2[Term, Rhs], opspecs: ListSet[OperationSpec], count: Int): String = {
      val start = "(test \"test" + count + getOpName(pair._1) + "\" "
      val leftReturnType = getReturnType(pair._1, opspecs)
      // have to match the right side first to determine how to
      // create a relevant and correct test
      val eqRightSide = pair._2 match {
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
        case rhsID: RhsID => "(= " + rhsID.toSexpr
      }

      // wraps the left side if necessary
      val eqLeftSide = pair._1 match {
        case id: TermID => throw new RuntimeException("can't have an ID here")
        case term: TermExpr => wrapTerm(term, opspecs, leftReturnType)
      }

      return start + eqRightSide + " " + eqLeftSide + "))\n"
    }

    // determines the compare operator that should be in front of the primitive
    // expression depending on its return type
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

    // wraps an Rhs in an operation that returns a primitive type so that
    // we can test it
    def wrapRhsExpr(expr: RhsExpr, opspecs: ListSet[OperationSpec], targetReturnType: Terminal): String = {
      val returnType = getReturnType(expr, opspecs)

      returnType match {
        case typeLit: TypeLiteral => return expr.toSexpr
        case typeName: TypeName => return findRhsWrapper(expr, targetReturnType, typeName, opspecs)
      }

    }

    // helper function to get the return type of a term based on it's op
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


    // helper function to get the return type of an Rhs based on it's op
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

    // wraps a term in an operation that returns a primitive type so that
    // we can test against it
    def wrapTerm(term: TermExpr, opspecs: ListSet[OperationSpec], targetReturnType: Terminal): String = {
      val returnType = getReturnType(term, opspecs)

      returnType match {
        case typeLit: TypeLiteral => return term.toSexpr
        case typeName: TypeName => return findTermWrapper(term, targetReturnType, typeName, opspecs)
      }
    }

    // finds the correct wrapper for the given type name from the list of opspecs
    def findRhsWrapper(expr: RhsExpr, returnType: Terminal, typeName: TypeName, opSpecs: ListSet[OperationSpec]): String = {
      // look through opspecs for one that takes the type name and returns
      // the return type
      for (opSpec <- opSpecs) {
        opSpec.returnType match {
          case lit: TypeLiteral => {
            val opArgs = opSpec.getArgTypes
            if (opArgs.length == 1 && opArgs.contains(typeName) && lit == returnType) {
              return "(" + opSpec.op + " " + expr.toSexpr + ")"
            }
          }
          case _ =>
        }
      }
      // cant find a wrapper so we should throw the test away
      throw new RuntimeException("Can't find a wrapper.")
    }

    // if a term doesn't return a primitive, then we have
    // to wrap it appropriately 
    def findTermWrapper(expr: TermExpr, returnType: Terminal, typeName: TypeName, opSpecs: ListSet[OperationSpec]): String = {
      // look through opspecs for one that takes the type name and returns
      // the return type
      for (opSpec <- opSpecs) {
        opSpec.returnType match {
          case lit: TypeLiteral => {
            val opArgs = opSpec.getArgTypes
            if (opArgs.length == 1 && opArgs.contains(typeName) && lit == returnType) {
              return "(" + opSpec.op + " " + expr.toSexpr + ")"
            }
          }
          case _ =>
        }
      }
      // cant find a wrapper so we should throw the test away
      throw new RuntimeException("Can't find a wrapper.")
    }

    // takes a term and returns it's operation as a string
    def getOpName(term: Term): String = {
      term match {
        case termExpr: TermExpr => termExpr.op.toString
        case termID: TermID => ""
      }
    }
  }
}
