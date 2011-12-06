package com.cpb.cs4500.valueGeneration {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.rewriting.Rewriter
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.HashSet
  import scala.collection.immutable.ListSet

  class ValueGenerator(specification: Spec) {

    // Store the basic constructors so we can create them on the fly
    val constructMap: HashMap[TypeName, ListSet[OperationSpec]] = createConstructorMap()

    val basicCreatorMap: HashMap[TypeName, ListSet[OperationSpec]] = createBasicCreatorMap()



    // Store the generated TypeLiterals here for replacement with the 
    val valMap = HashMap[TermID, Term]()

    val generatedValues = HashSet[Term]()

    def createTestsForADT() = {
      null
    }

    def createBasicCreatorMap(): HashMap[TypeName, ListSet[OperationSpec]] = {
      val bcMap = HashMap[TypeName, ListSet[OperationSpec]]()

      for ((key, value) <- constructMap) {
        val opSpecList: ListSet[OperationSpec] = value
        var creatorList = ListSet[OperationSpec]()

        for (opspec <- opSpecList)
          if (opspec.isBasicCreator()) creatorList = creatorList + opspec

        bcMap.put(key, creatorList)
      }

      bcMap
    }

    // create a term for this opspec
    def createTestsForOpSpec(opspec: OperationSpec): ListSet[Term] = {
      var tests = ListSet[Term]()
      val argTypes: List[Terminal] = opspec.getArgTypes

      for (argType <- argTypes) {
        tests = tests + createTermForArg(argType)
      }

      tests
    }

    def createTermForArg(argType: Terminal): Term = {
      argType match {
        case typeName: TypeName => createTermForTypeName(typeName)
        case typeLit: TypeLiteral => createValueForTypeLit(typeLit)
      }
    }

    def createTermForTypeName(typeName: TypeName): Term = {
      null
    }

    def createValueForTypeLit(typeLit: TypeLiteral): Term = {
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
    def createConstructorMap(): HashMap[TypeName, ListSet[OperationSpec]] = {
      specification.getAllConstructors()
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

