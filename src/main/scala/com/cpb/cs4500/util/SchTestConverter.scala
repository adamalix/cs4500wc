/**
 * Class used to convert tuples of left and rhs to a test Sexpr
 */

package com.cpb.cs4500.util {
  import com.cpb.cs4500.parsing._

  import scala.collection.immutable.ListSet

  object SchTestConverter {

    // converts the term and it's rewritten counterpart into a test expression
    def createTestSexpr(pair: Tuple2[Term, Rhs], count: Int, opspecs: ListSet[OperationSpec]): String = {
      "(test " + "\"test" + count + getOpName(pair._1)  + "\" " +
      tupleToTest(pair, opspecs) + ")"
    }

    def tupleToTest(pair: Tuple2[Term, Rhs], opspecs: ListSet[OperationSpec]): String = {
      // we need to know if we need to wrap the values or just return
      // their sexprs. this depends on the rhs value return type
      val eqRightSide = pair._2 match {
        case rhsFalse: RhsFalse => return "(not " + pair._1.toSexpr
        case rhsTrue: RhsTrue => return pair._1.toSexpr
        case int: RhsUInt => "(= " + int
        // Interesting cases
        case rhsExpr: RhsExpr => findWrapperForOp(rhsExpr.op, opspecs)
        case primExpr: RhsPrimExpr => primExpr.prim match {
          // All the boolean primExprs dont' need to be compared,
          // just evaluated
          case gt: GreaterThan => return pair._2.toSexpr
          case lt: LessThan => return pair._2.toSexpr
          case eq: Eq => return pair._2.toSexpr
          case not: Not => return pair._2.toSexpr
          // default to wrapping in "(= "
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
          findWrapperForOp(expr.op, opspecs).toString
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
      for (opSpec <- opSpecs) {
        if (opSpec.returnType == returnType) {
          returnType match {
            case lit: TypeLiteral => lit match {
              case num: IntLiteral => return "(= "
              // hack
              case bool: BooleanLiteral => "bool"
              case char: CharLiteral => "(char=? "
              case str: StringLiteral => "(string=? "
            }
          }
        }
      }
      throw new RuntimeException("No OpSpec returns this type, Impossible.")
    }


    def getOpName(term: Term): String = {
      term match {
        case termExpr: TermExpr => termExpr.op.toString
        case termID: TermID => ""
      }
    }

  }
}
