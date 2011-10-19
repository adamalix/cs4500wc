package com.cpb.cs4500.parsing {

  trait Terminal {
    override def toString():String
  }

  case class Spec(signatures:ADTSignatures, equations:Equation) {
    override def toString():String = {
        signatures.toSexpr + equations.toString
    }
  }

  case class ADTSignatures(sigs:List[ADTSignature]) {
    def toSexpr():String = {
        var sexpr:String = ""
        sigs.foreach((sig:ADTSignature) => sexpr+=sig.toSexpr + "\n")
        sexpr
    }
  }

  case class ADTSignature(name:TypeName, opSpecs:OperationSpecs) {
    def toSexpr():String = {
        "ADT named: " + name.toString + "\n" + opSpecs.toSexpr
    }
  }

  case class OperationSpecs(ops:List[OperationSpec]) {
    def toSexpr():String = {
        var sexpr:String = ""
        ops.foreach((op:OperationSpec) => sexpr+= op.toSexpr + "\n")
        sexpr
    }
  }

  case class OperationSpec(op:Operation, argTypes:ArgTypes, returnType:Terminal){
    def toSexpr():String = {
        "(test (" + op.toString + argTypes.toString + ") " + returnType.toString + ")"
    }
  }

  case class Operation(ident:String) extends Terminal {
    override def toString():String = ident
  }

  case class ArgTypes(args:List[Terminal]){
    override def toString():String = {
        var whole:String = ""
        args.foreach((arg:Terminal) => whole+= " " + arg.toString)
        whole
    }
  }

  case class TypeLiteral(value:String) extends Terminal {
    override def toString():String = value
  }

  case class TypeName(value:String) extends Terminal {
    override def toString():String = value
  }

  case class Equation(value:String) {
    override def toString():String = value
  }

}
