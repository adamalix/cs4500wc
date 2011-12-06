package com.cpb.cs4500.valueGeneration {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.rewriting.Rewriter
  import scala.collection.mutable.Map
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.HashSet
  import scala.collection.immutable.ListSet

  class ValueGenerator(specification: Spec) {

    // Store the basic constructors so we can create them on the fly
    val constructMap: Map[TypeName, List[OperationSpec]] = createConstructorMap()

    val basicCreatorMap: Map[TypeName, List[OperationSpec]] = createBasicCreatorMap()



    // Store the generated TypeLiterals here for replacement with the
    val valMap = Map[TermID, Term]()

    val generatedValues = HashSet[Term]()

    def createTestsForADT() = {
      null
    }

    def createBasicCreatorMap(): Map[TypeName, List[OperationSpec]] = {
      val bcMap = Map[TypeName, List[OperationSpec]]()

      for ((key, value) <- constructMap) {
        val opSpecList: List[OperationSpec] = value
        var creatorList = List[OperationSpec]()

        for (opspec <- opSpecList)
          if (opspec.isBasicCreator()) creatorList = opspec :: creatorList

        bcMap += (key -> creatorList)
      }

      bcMap
    }

    // create a term for this opspec
    def createTestsForOpSpec(opspec: OperationSpec, numTests: Int): ListSet[Term] = {
      val argTypes: List[Terminal] = opspec.getArgTypes
      if (argTypes.isEmpty)
          return ListSet(TermExpr(opspec.op, EmptyArg()))
      var tests = ListSet[Term]()

      // while loop
      createTestForOp(opspec.op, argTypes)

      tests
    }

    def createTestForOp(op: Operation, argTypes: List[Terminal]): Term = {
      TermExpr(op, createArgs(argTypes))
    }

    def createArgs(argTypes: List[Terminal]): Arg = {
      if (argTypes.isEmpty)
        return EmptyArg()

      val term = argTypes.head match {
        case literal: TypeLiteral => createTermAndValueForTypeLit(literal)
        case typeName: TypeName => createTermForTypeName(typeName)
      }

      Args(term, createArgs(argTypes.tail))
    }

    def createTermForTypeName(typeName: TypeName): Term = {
      val basicCreators: List[OperationSpec] = basicCreatorMap(typeName)

      // wtf happens here
      if (basicCreators.isEmpty)
        throw new RuntimeException("Cannot instantiate ADT: " + typeName +
                                   " because it has no basic creator")

      // TODO: DONT DO THIS I NTHE FUTURE
      val idx = scala.util.Random.nextInt(basicCreators.size)
      val randomCreator = basicCreators(idx)
      createTestForOp(randomCreator.op, randomCreator.getArgTypes())
    }

    def createTermAndValueForTypeLit(typeLit: TypeLiteral): Term = {
      null
    }

    def createAllTests(): List[Terminal] = {
      var allTests: List[Terminal] = List[Terminal]()
      allTests
    }

    def constructTests(opspec: OperationSpec): List[Term] = {
      var generatedTests: List[Term] = List[Term]()
      generatedTests
    }

    // Get a map of all the Constructors
    def createConstructorMap(): Map[TypeName, List[OperationSpec]] = {
      val map: HashMap[TypeName, ListSet[OperationSpec]] = specification.getAllConstructors()
      val destMap = Map[TypeName, List[OperationSpec]]()
      for ((key, value) <- map) {
        var list = List[OperationSpec]()
        value.foreach((op: OperationSpec) => list = op :: list)
        //var list: List[OperationSpec] = value ++: List[OperationSpec]()
        destMap += (key -> list)
      }
      destMap
    }

    def generateRandomIntLiteral(): IntLiteral= {
      IntLiteral(scala.util.Random.nextInt(1001).toString)
    }

    def generateRandomBooleanLiteral(): BooleanLiteral= {
      BooleanLiteral(scala.util.Random.nextBoolean.toString)
    }

    def generateRandomCharLiteral(): CharLiteral= {
      CharLiteral(scala.util.Random.nextPrintableChar.toString)
    }

    def generateRandomStringLiteral(): StringLiteral= {
      val strList = List("string1", "string2", "string3", "string4", "string5")
      StringLiteral(strList(scala.util.Random.nextInt(5)))
    }

    def generateRandomMapKey(): String = {
      scala.util.Random.nextInt(10000000).toString()
    }

    def generateTerm(): TermID = {
      new TermID(scala.util.Random.nextString(12))
    }

  }

}

/*
    def generatedFunctionValues(opspec:OperationSpec) : GeneratedFunction = {
      val operationName:String = opspec.getOpName
      val returnLiteral:Terminal = opspec.returnType
      val arguments:List[Terminal] = opspec.argTypes.args
      var listGenValues:List[GeneratedValue] = List[GeneratedValue]()
      for (arg<-arguments) {
        arg match {
          case intLit:IntLiteral => listGenValues = listGenValues :+ (new GeneratedPrimitive(generateRandomIntLiteral()))
          case boolLit:BooleanLiteral => listGenValues = listGenValues :+ (new GeneratedPrimitive(generateRandomBooleanLiteral()))
          case charLit:CharLiteral => listGenValues = listGenValues :+ (new GeneratedPrimitive(generateRandomCharLiteral()))
          case stringLit:StringLiteral => listGenValues = listGenValues :+ (new GeneratedPrimitive(generateRandomStringLiteral()))
          //case typeN:TypeName => generateTypeNameValues(typeN, listGenValues)
        }
      }
      new GeneratedFunction(operationName, listGenValues, returnLiteral)
    }

    // Given an OperationSpec, figure out how many tests
    // we need to make for it (based on the arguments).
    def countNumberOfTests(opspec:OperationSpec):Int = {
      var totalTests:Int = 1
      val arguments:List[Terminal] = opspec.argTypes.args
      for (arg<-arguments) {
        arg match {
          case typeN:TypeName => totalTests *= TypeToConstructer.get(typeN).size
        }
      }
      totalTests
    }

    def createCorrectNumberOfTests(opspec:OperationSpec):List[GeneratedFunction] = {
      val totalTestsToMake:Int = countNumberOfTests(opspec)
      var generatedTests:List[GeneratedFunction] = List[GeneratedFunction]()
      for (i <- 0 until totalTestsToMake) {
        System.out.println("HNNNG")
      }
      generatedTests

       val operationName:String = opspec.getOpName
       val returnLiteral:Terminal = opspec.returnType
       val arguments:List[Terminal] = opspec.argTypes.args
       var listGenValues:List[GeneratedValue] = List[GeneratedValue]()
       for (arg<-arguments)
       {
       arg match{
       case intLit:IntLiteral => listGenValues = listGenValues :+ (new GeneratedPrimitive(generateRandomIntLiteral()))
       case boolLit:BooleanLiteral => listGenValues = listGenValues :+ (new GeneratedPrimitive(generateRandomBooleanLiteral()))
       case charLit:CharLiteral => listGenValues = listGenValues :+ (new GeneratedPrimitive(generateRandomCharLiteral()))
       case stringLit:StringLiteral => listGenValues = listGenValues :+ (new GeneratedPrimitive(generateRandomStringLiteral()))
       //case typeN:TypeName => generateTypeNameValues(typeN, listGenValues)
       }
       }
       new GeneratedFunction(operationName, listGenValues, returnLiteral)

    }
    def generateRandomIntLiteral():IntLiteral= {
      IntLiteral(scala.util.Random.nextInt(1001).toString)
    }

    def generateRandomBooleanLiteral():BooleanLiteral= {
      BooleanLiteral(scala.util.Random.nextBoolean.toString)
    }

    def generateRandomCharLiteral():CharLiteral= {
      CharLiteral(scala.util.Random.nextPrintableChar.toString)
    }

    def generateRandomStringLiteral():StringLiteral= {
      val strList = List("string1", "string2", "string3", "string4", "string5")
      StringLiteral(strList(scala.util.Random.nextInt(5)))
    }

  }
}*/

