package com.cpb.cs4500.parsing {

  import scala.util.parsing.combinator._

  class ADTParser extends JavaTokenParsers {
    override def skipWhitespace = true

    def spec: Parser[Any] = "Signatures:" ~ adtSignatures ~ "Equations:" ~ equations

    def adtSignatures: Parser[Any] = adtSignature | adtSignature ~ adtSignatures

    def adtSignature: Parser[Any] = "ADT:" ~ typeName ~ operationSpecs

    def operationSpecs: Parser[Any] = operationSpec | operationSpec ~ operationSpecs

    def operationSpec: Parser[Any] = (
      operation ~ ":" ~ "->" ~ typeLiteral
      | operation ~ ":" ~ argTypes  ~ "->" ~ typeLiteral
    )

      def operation: Parser[Any] = ident


    def argTypes: Parser[ArgTypes] = typeLiteral ~ "*"  ~ argTypes | typeLiteral

    def typeLiteral: Parser[TypeLiteral] = (
      (("int" | "boolean" | "character" | "string" ) ^^ (new TypeLiteral(_)))
      | typeName)

      def typeName: Parser[TypeLiteral] = ident ^^ (new TypeLiteral(_))

    def equations: Parser[Any] = ""
  }


}
