package com.cpb.cs4500.valueGeneration {
    import com.cpb.cs4500.parsing._
    import scala.collection.mutable.HashMap
    import scala.collection.immutable._
    import scala.util._
    object ValueGenerator{
      
      
      //TODO
      /*
      CallOpSpecReplacement on al opSpecs
        - Get AlTypes
        - Get All Constructors
      */
      def opSpecReplacement(op:OperationSpec, 
                            allTypes:List[Terminal], 
                            valMap:HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]],
                            constMap:HashMap[TypeName, OperationSpec]):HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]] =
      {
        if(op.basicCreator)
        {
            valMap.put(op, List((op.argTypes, TypeLiteral(op.getOpName))))
            valMap
        }
        else
        {
            var argVals:List[Terminal] = List[Terminal]()
            for(term<-op.argTypes.args)
            {
                  argVals = argVals :+ generateValueLiteral(term, constMap)
            }
            System.out.println(argVals.toString)
            var returnVal:TypeLiteral = TypeLiteral("temporary")
            op.returnType match {
                case lit:TypeLiteral => returnVal = generateValueLiteral(lit, constMap)
                case name:TypeName => returnVal = generateValueLiteral(name, constMap)
            }
            valMap.put(op, List((ArgTypes(argVals), returnVal)))
            valMap
        }
      }

      def generateValueLiteral(term:Terminal, constMap:HashMap[TypeName, OperationSpec]):TypeLiteral = 
      {
        term match {
            case lit:TypeLiteral => 
            {
            System.out.println(lit.value)
                lit.value match {
                    case "int" => generateRandomIntLiteral()
                    case "boolean" => generateRandomBoolean()
                    case "character" => generateRandomCharacter()
                    case "string" => generateRandomString()
                    }
            }
            case name:TypeName => 
            {
                val const:String = constMap(name).getOpName
                TypeLiteral(const)
            }
        }
      }
      
      def generateRandomIntLiteral():TypeLiteral= 
      {
        TypeLiteral(scala.util.Random.nextInt(1001).toString)
      }
      
      def generateRandomBoolean():TypeLiteral=
      {
        TypeLiteral(scala.util.Random.nextBoolean.toString)
      }
      
      def generateRandomCharacter():TypeLiteral=
      {
         TypeLiteral(scala.util.Random.nextPrintableChar.toString)
      }
      
      def generateRandomString():TypeLiteral=
      {
         val strList = List("string1", "string2", "string3", "string4", "string5")
         TypeLiteral(strList(scala.util.Random.nextInt(5)))
      }
      
      def generateTypeNameValue(name:String):TypeLiteral = {
         TypeLiteral("make-" + name)
      }
      
      
      /*
      def createLiteralValues(lit:TypeLiteral) =
      {
         val litVal:String = lit.toString
         if(litVal.equals("int"))
            valueMap.put( litVal, "-1" )
         else if(litVal.equals("character"))
            valueMap.put(litVal, "#\\a")
         else if(litVal.equals("boolean"))
            valueMap.put(litVal, "true")
         else
            valueMap.put(litVal, "testString")
      }
      */
    
    }
}
