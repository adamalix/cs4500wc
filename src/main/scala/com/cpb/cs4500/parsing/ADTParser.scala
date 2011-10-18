package com.cpb.cs4500.parsing {

  import scala.util.parsing.combinator._

  class ADTParser extends JavaTokenParsers {
    override def skipWhitespace = true

    val argTypeLiterals = List[TypeLiteral]()

    def spec: Parser[Spec] = (
      "Signatures:" ~ adtSignatures ~ equations ^^
      { case "Signatures:" ~ adtSigs ~ eqs => Spec(adtSigs, eqs) }
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
          OperationSpec(op, null, returnType) }

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

    def equations: Parser[Equation] = (
      "Equations:" ^^ { case eqs => Equation(eqs) }
    )
  }

}
