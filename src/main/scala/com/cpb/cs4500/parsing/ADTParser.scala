/*
ADTParser.scala
Parses the given ADT and converts the tokens into readable data based on the case classes
*/

package com.cpb.cs4500.parsing {

  import scala.util.parsing.combinator._

  class ADTParser extends JavaTokenParsers {
    override def skipWhitespace = true

    val argTypeLiterals = List[TypeLiteral]()

    def spec: Parser[Spec] = (
      "Signatures:" ~ adtSignatures ~ "Equations:" ~ equations ^^
      { case "Signatures:" ~ adtSigs ~ "Equations:" ~ eqs => Spec(adtSigs, eqs) }
    )

    def adtSignatures: Parser[ADTSignatures] = (
      rep(adtSignature) ^^ { case adtSigList => ADTSignatures(adtSigList) }
    )

    def adtSignature: Parser[ADTSignature] = (
      "ADT:" ~> typeName ~ operationSpecs ^^
      { case namedType ~ opSpecs => ADTSignature(namedType, opSpecs) }
    )

    def operationSpecs: Parser[OperationSpecs] = (
      rep(operationSpec) ^^ { case opSpecList => OperationSpecs(opSpecList) }
    )

    def operationSpec: Parser[OperationSpec] = (
        // Most likely a basic creator
        operation ~ ":" ~ "->" ~ typeLiteral ^^
        { case op ~ ":" ~ "->" ~ returnType =>
          OperationSpec(op, new ArgTypes(List()), returnType, true) }

      | operation ~ ":" ~ argTypes ~ "->" ~ typeLiteral ^^
        { case op ~ ":" ~ args ~ "->" ~ returnType => opSpecHelper(op, args, returnType) }
    )

    // detect whether we have a basic creator on our hands!
    def opSpecHelper(op: Operation, args: ArgTypes, returnType: Terminal): OperationSpec = {
      val isBasicCreator = returnType match {
        case typeName: TypeName => !args.getAllTypeNames().contains(typeName)
        case _ => false
      }

      OperationSpec(op, args, returnType, isBasicCreator)

    }

    def operation: Parser[Operation] = ident ^^ { case op => Operation(op) }

    def argTypes: Parser[ArgTypes] = (
      repsep(typeLiteral, "*") ^^
      { case argTypesList => ArgTypes(argTypesList) }
    )

    def typeLiteral: Parser[Terminal] = (
        "int" ^^ { case literalType => IntLiteral(literalType) }
      | "boolean" ^^ { case literalType => BooleanLiteral(literalType) }
      | "character" ^^ { case literalType => CharLiteral(literalType) }
      | "string" ^^ { case literalType => StringLiteral(literalType) }
      | typeName
    )

    def typeName: Parser[TypeName] = ident ^^
      { case namedType => TypeName(namedType) }

    def equations: Parser[Equations] = (
      rep(equation) ^^ { case eqsList => Equations(eqsList) }
    )

    def equation: Parser[Equation] = (
      term ~ "=" ~ rhs ^^ { case left ~ "=" ~ right => Equation(left, right) }
    )

    def term: Parser[Term] = (
        "(" ~> operation ~ args <~ ")" ^^ { case op ~ args => TermExpr(op, args) }
      | ident ^^ { case identifier => TermID(identifier) }
    )

    def args: Parser[Arg] = (
        term ~ args ^^ { case term ~ args => Args(term, args) }
      | emptyArg

    )

    // Hack because there is not else case
    def emptyArg: Parser[Arg] = (
      rep("BOGUS&123456789") ^^ { case _ => EmptyArg() }
    )

    def rhs: Parser[Rhs] = (
        "#t" ^^ { case itsTrue => RhsTrue() }
      | "#f" ^^ { case itsFalse => RhsFalse() }
      | decimalNumber ^^ { case number => RhsUInt(number) }
      | ident ^^ { case identifier => RhsID(identifier) }
      | "(" ~> operation ~ rhsArgs <~ ")" ^^
        { case op ~ rhsArgs => RhsExpr(op, rhsArgs) }
      | "(" ~> primOp ~ rhsArgs <~ ")" ^^
        { case primOp ~ rhsArgs => RhsPrimExpr(primOp, rhsArgs) }
    )

    def rhsArgs: Parser[RhsArg] = (
        rhs ~ rhsArgs ^^ { case rhs ~ rhsArgs => RhsArgs(rhs, rhsArgs) }
      | emptyRhsArg
    )

    def emptyRhsArg: Parser[RhsArg] = (
      rep("BOGUS&123456789") ^^ { case _ => RhsEmptyArg() }
    )

    def primOp: Parser[Primitive] = (
        "not" ^^ { case not => Not() }
      | "+" ^^ { case plus => Plus() }
      | "-" ^^ { case minus => Minus() }
      | "*" ^^ { case star => Star() }
      | "=" ^^ { case eq => Eq() }
      | ">" ^^ { case gt => GreaterThan() }
      | "<" ^^ { case lt => LessThan() }
    )

  }

}
