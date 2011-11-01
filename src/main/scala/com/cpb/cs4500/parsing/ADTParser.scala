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
        operation ~ ":" ~ "->" ~ typeLiteral ^^
        { case op ~ ":" ~ "->" ~ returnType =>
          OperationSpec(op, new ArgTypes(List()), returnType) }

      | operation ~ ":" ~ argTypes ~ "->" ~ typeLiteral ^^
        { case op ~ ":" ~ args ~ "->" ~ returnType =>
          OperationSpec(op, args, returnType) }
    )

    def operation: Parser[Operation] = ident ^^ { case op => Operation(op) }

    def argTypes: Parser[ArgTypes] = (
      repsep(typeLiteral, "*") ^^
      { case argTypesList => ArgTypes(argTypesList) }
    )

    def typeLiteral: Parser[Terminal] = (
        (("int" | "boolean" | "character" | "string" ) ^^
         { case literalType => TypeLiteral(literalType) })
      | typeName
    )

    def typeName: Parser[TypeName] = ident ^^
      { case namedType => TypeName(namedType) }

    def equations: Parser[Equations] = (
      rep(equation) ^^ { case eqsList => Equations(eqsList) }
    )

    def equation: Parser[Equation] = (
      term ~ "=" ~ term ^^ { case left ~ "=" ~ right => Equation(left, right) }
    )

    def term: Parser[Term] = (
        "(" ~> operation ~ rep(arg) <~ ")" ^^ { case op ~ args => Term("", op, args) }
      | ident ^^ { case identifier => Term(identifier, Operation(""), List()) }
    )

    def arg: Parser[Arg] = (
      term ~ rep(arg) ^^ { case term ~ args => Arg(term, args) }
      | rep(arg) ^^ { case emptyList => Arg(Term("empty", Operation(""), emptyList), emptyList) }
    )

  }

}
