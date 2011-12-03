package com.cpb.cs4500.valueGeneration {
    import scala.util.parsing.combinator._
    import com.cpb.cs4500.parsing._
    import com.cpb.cs4500.rewriting._
    import com.cpb.cs4500.io._
    import scala.collection.mutable._
    import scala.util._
    

    object ValueGenerator{
    //Just a reference
    
        var TypeToConstructer:HashMap[Int, String] = HashMap[Int, String]()
        def fillTypeMap(spec:Spec):Unit = 
        {
            System.out.println("HNNNNNG")
        }
        
        def generatedFunctionValues(opspec:OperationSpec) : GeneratedFunction =
        {
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
        
        def generateRandomIntLiteral():IntLiteral= 
        {
            IntLiteral(scala.util.Random.nextInt(1001).toString)
        }
      
        def generateRandomBooleanLiteral():BooleanLiteral=
        {
            BooleanLiteral(scala.util.Random.nextBoolean.toString)
        }
      
        def generateRandomCharLiteral():CharLiteral=
        {
            CharLiteral(scala.util.Random.nextPrintableChar.toString)
        }
      
        def generateRandomStringLiteral():StringLiteral=
        {
            val strList = List("string1", "string2", "string3", "string4", "string5")
            StringLiteral(strList(scala.util.Random.nextInt(5)))
        }
        
    }
}
