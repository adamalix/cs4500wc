class Spec(sigs:ADTSignatures, eqs:Equation) {}

class ADTSignatures(sigs:List[ADTSignature]) {}

class ADTSignature(name:TypeName, opSpecs:OperationSpecs) {}

class OperationSpecs(ops:List[OperationSpec]) {}

class OperationSpec(op:Operation, argTypes:ArgTypes, typeLit:TypeLiteral){}

class Operation(ident:String) {}

class ArgTypes(args:List[TypeLiteral]){}

class TypeLiteral(value:String) {}

class TypeName(value:String) {}

class Equation(value:String) {}

