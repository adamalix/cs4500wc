package com.cpb.cs4500.parsing {

  trait Terminal {}

  case class Spec(sigs:ADTSignatures, eqs:Equation) {}

  case class ADTSignatures(sigs:List[ADTSignature]) {}

  case class ADTSignature(name:TypeName, opSpecs:OperationSpecs) {}

  case class OperationSpecs(ops:List[OperationSpec]) {}

  case class OperationSpec(op:Operation, argTypes:ArgTypes, returnType:Terminal){}

  case class Operation(ident:String) extends Terminal {}

  case class ArgTypes(args:List[Terminal]){}

  case class TypeLiteral(value:String) extends Terminal {}

  case class TypeName(value:String) extends Terminal {}

  case class Equation(value:String) {}

}
