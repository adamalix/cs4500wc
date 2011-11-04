package com.cpb.cs4500.valueGeneration {
    import com.cpb.cs4500.parsing._
    import scala.collection.mutable.HashMap
    import scala.collection.immutable._
    import scala.util._
    object ValueGenerator{
      
      // Our value map. The Key is the Type and the Value is
      // a list of Strings (So we can have more than one value).
      //val valueMap = new HashMap[OpSpec, 
      
      /*
      // Creates our values
      def fillValueMap(spec:Spec) = 
      {
          val allTypeLiterals:ListSet[TypeLiteral] = spec.getAllTypeLiterals
          val allTypeNames:ListSet[TypeName] = spec.getAllTypeNames
          
          for(typeLit<-allTypeLiterals)
          {
             createLiteralValues(typeLit)
          }
          // What are we returning here? - AA
      }
      
      
        */
      def opSpecReplacement(op:OperationSpec, 
                            allTypes:List[Terminal], 
                            valMap:HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]]):HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]] =
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
                  argVals = argVals :+ generateValueLiteral(term)
            }
            System.out.println(argVals.toString)
            var returnVal:TypeLiteral = TypeLiteral("Check")
            op.returnType match {
                case lit:TypeLiteral => returnVal = generateValueLiteral(lit)
                case name:TypeName => returnVal = TypeLiteral("MERP")
            }
            valMap.put(op, List((ArgTypes(argVals), returnVal)))
            valMap
        }
        
        //for(terminal<-op.argTypes.args)
      }

      def generateValueLiteral(term:Terminal):TypeLiteral = 
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
            case name:TypeName => TypeLiteral("TEST") 
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
         TypeLiteral(scala.util.Random.nextString(scala.util.Random.nextInt(11)))
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
