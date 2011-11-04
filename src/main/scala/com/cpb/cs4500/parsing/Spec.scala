/*
 Spec.scala is the home of many of our internal representations.
 After our parser goes through the input, it inserts them into their
 appropriate internal representation. Many of these have toStrings/toSexprs
 in order to be used in the Rewriter.
 */

package com.cpb.cs4500.parsing {
  import scala.collection.immutable._
  
  trait Terminal {
    override def toString():String
  }

  case class Spec(signatures:ADTSignatures, equations:Equations) 
  {
    override def toString():String = {
      signatures.toSexpr
    }
    
    def getAllTypeNames():ListSet[TypeName] = {
        signatures.getAllTypeNames()
    }
    
    def getAllTypeLiterals():ListSet[TypeLiteral] = {
        signatures.getAllTypeLiterals()
    }
    
    def getAllTypes():ListSet[Terminal] = {
        signatures.getAllTypeNames() ++ signatures.getAllTypeLiterals()
    }
    
    def getAllOpSpecs():ListSet[OperationSpec] = {
        signatures.getAllOpSpecs
    }
    
    def getOperationNames():List[String] = {
        val ops:ListSet[OperationSpec] = getAllOpSpecs
        var opNameList:List[String] = List[String]()
        
        for (op<-ops){
            opNameList = opNameList :+ op.getOpName
        }
        opNameList
        
    }
  }

  case class ADTSignatures(sigs:List[ADTSignature])  
  {
    def toSexpr():String = {
      var sexpr:String = ""
      sigs.foreach((sig:ADTSignature) => sexpr+=sig.toSexpr)
      sexpr
    }
    
    def getAllTypeNames():ListSet[TypeName] = {
        var allTypeNames:ListSet[TypeName] = new ListSet[TypeName]()
        for (sig<-sigs){
            allTypeNames = allTypeNames ++ sig.getAllTypeNames
        }
        allTypeNames    
    }
    
    def getAllTypeLiterals():ListSet[TypeLiteral] = {
        var allTypeLits:ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
        for (sig<-sigs){
            allTypeLits = allTypeLits ++ sig.getAllTypeLiterals
        }
        allTypeLits   
    }
    
    def getAllOpSpecs():ListSet[OperationSpec] = {
        var AllOpSpecs:ListSet[OperationSpec] = new ListSet[OperationSpec]()
        for (sig<-sigs){
            AllOpSpecs = AllOpSpecs ++ sig.getAllOpSpecs
        }
        AllOpSpecs
    }
  }
  
  case class ADTSignature(name:TypeName, opSpecs:OperationSpecs) {
    def toSexpr():String = {
      opSpecs.toSexpr
    }
    
    def getAllTypeNames():ListSet[TypeName] = {
      var allTypeNames:ListSet[TypeName] = new ListSet[TypeName]()
      allTypeNames = allTypeNames + name ++ opSpecs.getAllTypeNames
      allTypeNames
    }
    
    def getAllTypeLiterals():ListSet[TypeLiteral] = {
      var allTypeLits:ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
      allTypeLits = allTypeLits ++ opSpecs.getAllTypeLiterals
      allTypeLits
    }
    
    def getAllOpSpecs():List[OperationSpec] = {
        opSpecs.getOpSpecs
    }
  }

  case class OperationSpecs(ops:List[OperationSpec]) {
    def toSexpr():String = {
      var sexpr:String = ""
      ops.foreach((op:OperationSpec) => sexpr+= op.toSexpr + "\n")
      sexpr
    }
    
    def getAllTypeNames():ListSet[TypeName] = {
      var allTypeNames:ListSet[TypeName] = new ListSet[TypeName]()
      for(op<-ops){
         allTypeNames = allTypeNames ++ op.getAllTypeNames()
      }
      allTypeNames
    }
    def getAllTypeLiterals():ListSet[TypeLiteral] = {
      var allTypeLits:ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
      for(op<-ops){
         allTypeLits = allTypeLits ++ op.getAllTypeLiterals
      }
      allTypeLits
    }
    
    def getOpSpecs():List[OperationSpec] = {
        ops
    }
    
  }

  case class OperationSpec(op:Operation, argTypes:ArgTypes, returnType:Terminal) {
    def toSexpr():String = {
      "(test (" + op.toString + argTypes.toString + ") " + returnType.toString + ")"
    }
    
    def getAllTypeNames():ListSet[TypeName] = {
        var allTypeNames:ListSet[TypeName] = new ListSet[TypeName]()
        returnType match {
            case a:TypeName => allTypeNames = allTypeNames + a
            case a:TypeLiteral =>
            case a:Operation =>
        }
        allTypeNames = allTypeNames ++ argTypes.getAllTypeNames()
        allTypeNames
    }
    
    def getAllTypeLiterals():ListSet[TypeLiteral] = {
        var allTypeLits:ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
        returnType match {
            case a:TypeLiteral => allTypeLits = allTypeLits + a
            case a:TypeName => 
            case a:Operation =>
        }
        allTypeLits = allTypeLits ++ argTypes.getAllTypeLiterals()
        allTypeLits
    }
    
    def getOpName():String = {
        op.toString
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
    
    def getAllTypeNames():ListSet[TypeName] = {
      var allTypeNames:ListSet[TypeName] = new ListSet[TypeName]()
      for (a<-args) a match {
        case a:TypeName => allTypeNames = allTypeNames + a
        case a:TypeLiteral => 
        case a:Operation =>
      }
      allTypeNames
    }
        
    def getAllTypeLiterals():ListSet[TypeLiteral] = {
      var allTypeLits:ListSet[TypeLiteral] = new ListSet[TypeLiteral]()
        for (a<-args) a match {
            case a:TypeLiteral => allTypeLits = allTypeLits + a
            case a:TypeName => 
            case a:Operation =>
        }
        allTypeLits
    }
  }

  case class TypeLiteral(value:String) extends Terminal {
    override def toString():String = value
  }

  case class TypeName(value:String) extends Terminal {
  
    override def toString():String = value
  }

  case class Equations(eqs:List[Equation]) {
    //override def toString():String = value
  }

  case class Equation(left:Term, right:Term) {
    
  }

  case class Term(ident:String, op:Operation, args:List[Arg]) {
    
  }

  case class Arg(term:Term, args:List[Arg]) {
    
  }

}
