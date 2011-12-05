package com.cpb.cs4500.valueGeneration {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.rewriting.Rewriter
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.HashSet
  import scala.collection.immutable.ListSet

  class ValueGenerator(specification: Spec) {

    val spec = specification

    var generatedTerms = ListSet[Term]()

    val TypeToConstructer:HashMap[TypeName, List[GeneratedValue]] = HashMap[TypeName, List[GeneratedValue]]()



    def generateTerms(minTests: Int): ListSet[Term] = {
      generatedTerms
    }

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
      /*
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
       */
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
}
