/*
 Spec.scala is the home of many of our internal representations.
 After our parser goes through the input, it inserts them into their
 appropriate internal representation. Many of these have toStrings/toSexprs
 in order to be used in the Rewriter.
 */

package com.cpb.cs4500.parsing {
  import scala.collection.mutable.HashMap
  import scala.collection.immutable._


  trait Terminal {
    override def toString(): String
  }

  case class Spec(signatures: ADTSignatures, equations: Equations) {

    override def toString(): String = {
      signatures.toSexpr
    }

    def toGeneratedSexpr(repMap: HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]]): String = {
      signatures.toGeneratedSexpr(repMap)
    }

    def getAllTypeNames(): ListSet[TypeName] = {
      signatures.getAllTypeNames()
    }

    def getAllTypeLiterals(): ListSet[TypeLiteral] = {
      signatures.getAllTypeLiterals()
    }

    def getAllTypes(): ListSet[Terminal] = {
      signatures.getAllTypeNames() ++ signatures.getAllTypeLiterals()
    }

    def getAllOpSpecs(): ListSet[OperationSpec] = {
      signatures.getAllOpSpecs
    }

    def getAllConstructors(): HashMap[TypeName, ListSet[OperationSpec]] = {
      val allTypes: ListSet[TypeName] = getAllTypeNames()
      val allOpSpecs: ListSet[OperationSpec] = getAllOpSpecs()
      var allConstructors = HashMap[TypeName, ListSet[OperationSpec]]()

      for (typeN <- allTypes) {
        var constructors = ListSet[OperationSpec]()

        for (opspec <- allOpSpecs) {
          if (typeN.equals(opspec.getReturnType()))
            constructors = constructors + opspec
        }
        if (!constructors.isEmpty) {
          allConstructors.put(typeN, constructors)
        }
      }
      allConstructors
    }

    def getAllBaseConstructors(): ListSet[OperationSpec] = {
      var ops: ListSet[OperationSpec] = getAllOpSpecs
      var cons: ListSet[OperationSpec] = ListSet[OperationSpec]()
      for (op<-ops) {
        if (op.isBasicCreator) {
          cons = cons + op
        }
      }
      cons
    }

    def getOperationNames(): List[String] = {
      val ops: ListSet[OperationSpec] = getAllOpSpecs
      var opNameList: List[String] = List[String]()

      for (op<-ops) {
        opNameList = opNameList :+ op.getOpName
      }
      opNameList
    }
  }

  case class ADTSignatures(sigs: List[ADTSignature]) {
    def toSexpr(): String = {
      var sexpr: String = ""
      sigs.foreach((sig: ADTSignature) => sexpr+=sig.toSexpr)
      sexpr
    }

    def toGeneratedSexpr(repMap: HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]]): String = {
      var sexpr: String = ""
      sigs.foreach((sig: ADTSignature) => sexpr+=sig.toGeneratedSexpr(repMap))
      sexpr
    }

    def getAllTypeNames(): ListSet[TypeName] = {
      var allTypeNames: ListSet[TypeName] = new ListSet[TypeName]()
      for (sig<-sigs) {
        allTypeNames = allTypeNames ++ sig.getAllTypeNames
      }
      allTypeNames
    }

    def getAllTypeLiterals(): ListSet[TypeLiteral] = {
      var allTypeLits: ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
      for (sig<-sigs) {
        allTypeLits = allTypeLits ++ sig.getAllTypeLiterals
      }
      allTypeLits
    }

    def getAllOpSpecs(): ListSet[OperationSpec] = {
      var AllOpSpecs: ListSet[OperationSpec] = new ListSet[OperationSpec]()
      for (sig<-sigs) {
        AllOpSpecs = AllOpSpecs ++ sig.getAllOpSpecs
      }
      AllOpSpecs
    }
  }

  case class ADTSignature(name: TypeName, opSpecs: OperationSpecs) {
    def toSexpr(): String = {
      opSpecs.toSexpr
    }

    def toGeneratedSexpr(repMap: HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]]): String = {
      opSpecs.toGeneratedSexpr(repMap)
    }

    def getAllTypeNames(): ListSet[TypeName] = {
      var allTypeNames: ListSet[TypeName] = new ListSet[TypeName]()
      allTypeNames = allTypeNames + name ++ opSpecs.getAllTypeNames
      allTypeNames
    }

    def getAllTypeLiterals(): ListSet[TypeLiteral] = {
      var allTypeLits: ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
      allTypeLits = allTypeLits ++ opSpecs.getAllTypeLiterals
      allTypeLits
    }

    def getAllOpSpecs(): List[OperationSpec] = {
      opSpecs.getOpSpecs
    }
  }

  case class OperationSpecs(ops: List[OperationSpec]) {
    def toSexpr(): String = {
      var sexpr: String = ""
      ops.foreach((op: OperationSpec) => sexpr+= op.toSexpr + "\n")
      sexpr
    }

    def toGeneratedSexpr(repMap: HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]]): String = {
      var sexpr: String = ""
      ops.foreach((op: OperationSpec) => sexpr+= op.toGeneratedSexpr(repMap) + "\n")
      sexpr
    }

    def getAllTypeNames(): ListSet[TypeName] = {
      var allTypeNames: ListSet[TypeName] = new ListSet[TypeName]()
      for (op<-ops) {
        allTypeNames = allTypeNames ++ op.getAllTypeNames()
      }
      allTypeNames
    }

    def getAllTypeLiterals(): ListSet[TypeLiteral] = {
      var allTypeLits: ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
      for (op<-ops){
        allTypeLits = allTypeLits ++ op.getAllTypeLiterals
      }
      allTypeLits
    }

    def getOpSpecs(): List[OperationSpec] = {
      ops
    }

  }

  case class OperationSpec(op: Operation, argTypes: ArgTypes, returnType: Terminal, basicCreator: Boolean) {

    def toSexpr(): String = {
      "(test (" + op.toString + argTypes.toString + ") " + returnType.toString + ")"
    }

    def toGeneratedSexpr(repMap: HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]]): String = {
      val newArgTypes: ArgTypes = repMap(this).head._1
      val newReturnType: Terminal = repMap(this).head._2
      "(test (" + op.toString + newArgTypes.toString + ") " + newReturnType.toString + ")"
    }

    def getAllTypeNames(): ListSet[TypeName] = {
      var allTypeNames: ListSet[TypeName] = new ListSet[TypeName]()
      returnType match {
        case a: TypeName => allTypeNames = allTypeNames + a
        case a: TypeLiteral =>
        case a: Operation =>
      }
      allTypeNames = allTypeNames ++ argTypes.getAllTypeNames()
      allTypeNames
    }

    def getAllTypeLiterals(): ListSet[TypeLiteral] = {
      var allTypeLits: ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
      returnType match {
        case a: TypeLiteral => allTypeLits = allTypeLits + a
        case a: TypeName =>
          case a: Operation =>
      }
      allTypeLits = allTypeLits ++ argTypes.getAllTypeLiterals()
      allTypeLits
    }

    def getOpName(): String = {
      op.toString
    }

    def getReturnType(): Terminal = {
      returnType
    }

    def getArgTypes(): List[Terminal] = {
      argTypes.args
    }

    def isBasicCreator(): Boolean = {
      basicCreator
    }

  }

  case class Operation(ident: String) extends Terminal {
    override def toString(): String = ident
  }

  case class ArgTypes(args: List[Terminal]) {
    override def toString(): String = {
      var whole: String = ""
      args.foreach((arg: Terminal) => whole+= " " + arg.toString)
      whole
    }

    def getAllTypeNames(): ListSet[TypeName] = {
      var allTypeNames: ListSet[TypeName] = new ListSet[TypeName]()
      for (a<-args) a match {
        case a: TypeName => allTypeNames = allTypeNames + a
        case a: TypeLiteral =>
        case a: Operation =>
      }
      allTypeNames
    }

    def getAllTypeLiterals(): ListSet[TypeLiteral] = {
      var allTypeLits: ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
      for (a<-args) a match {
        case a: TypeLiteral => allTypeLits = allTypeLits + a
        case a: TypeName =>
          case a: Operation =>
      }
      allTypeLits
    }
  }

  abstract class TypeLiteral() extends Terminal

  case class IntLiteral(value: Int, stringValue: String) extends TypeLiteral {
    // additional constructors are for testing
    def this(stringValue: String) = this(-6789, stringValue)
    def this(value: Int) = this(value, value.toString)
    override def toString(): String = stringValue
  }

  case class BooleanLiteral(value: Boolean, stringValue: String) extends TypeLiteral {
    // additional constructors are for testing
    def this(stringValue: String) = this(false, stringValue)
    def this(value: Boolean) = this(value, value.toString)
    override def toString(): String = stringValue
  }

  case class CharLiteral(value: Char, stringValue: String) extends TypeLiteral {
    // additional constructors are for testing
    def this(stringValue: String) = this('c', stringValue)
    def this(value: Char) = this(value, value.toString)
    override def toString(): String = stringValue
  }

  case class StringLiteral(value: String) extends TypeLiteral {
    override def toString(): String = value
  }

  case class TypeName(value: String) extends Terminal {
    override def toString(): String = value
  }

  case class Equations(eqs: List[Equation])

  case class Equation(left: Term, right: Rhs)

  abstract class Term

  case class TermID(ident: String) extends Term

  case class TermExpr(op: Operation, args: Arg) extends Term

  // Lefthand side style Args
  trait Arg {
    def isEmpty(): Boolean = false
  }

  case class Args(term: Term, args: Arg) extends Arg

  case class EmptyArg() extends Arg {
    override def isEmpty(): Boolean = true
  }

  abstract class Rhs

  case class RhsTrue() extends Rhs {
    val value = "#t"
  }

  case class RhsFalse() extends Rhs {
    val value = "#f"
  }

  case class RhsUInt(value: String) extends Rhs

  case class RhsID(ident: String) extends Rhs

  case class RhsExpr(op: Operation, args: RhsArg) extends Rhs

  case class RhsPrimExpr(prim: Primitive, args: RhsArg) extends Rhs

  // Righthand side style Args
  trait RhsArg {
    def isEmpty(): Boolean = false
  }

  case class RhsArgs(rhs: Rhs, args: RhsArg) extends RhsArg

  case class RhsEmptyArg() extends RhsArg {
    override def isEmpty(): Boolean = true
  }

  abstract class Primitive

  case class Not() extends Primitive {
    val value = "not"
  }

  case class Plus() extends Primitive {
    val value = "+"
  }

  case class Minus() extends Primitive {
    val value = "-"
  }

  case class Star() extends Primitive {
    val value = "*"
  }

  case class Eq() extends Primitive {
    val value = "="
  }

  case class GreaterThan extends Primitive {
    val value = ">"
  }

  case class LessThan extends Primitive {
    val value = "<"
  }

}
