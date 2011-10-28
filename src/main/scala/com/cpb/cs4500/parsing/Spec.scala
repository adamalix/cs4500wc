/*
 Spec.scala is the home of many of our internal representations.
 After our parser goes through the input, it inserts them into their
 appropriate internal representation. Many of these have toStrings/toSexprs
 in order to be used in the Rewriter.
 */

package com.cpb.cs4500.parsing {

  trait Terminal {
    override def toString():String
  }

  case class Spec(signatures:ADTSignatures, equations:Equation) {
    override def toString():String = {
      signatures.toSexpr
    }
  }

  case class ADTSignatures(sigs:List[ADTSignature]) {
    def toSexpr():String = {
      var sexpr:String = ""
      sigs.foreach((sig:ADTSignature) => sexpr+=sig.toSexpr)
      sexpr
    }
  }

  case class ADTSignature(name:TypeName, opSpecs:OperationSpecs) {
    def toSexpr():String = {
      opSpecs.toSexpr
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
