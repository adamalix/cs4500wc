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
                            valMap:HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]]) =
      {
        if(op.basicCreator)
        {
            valMap.put(op, List((op.argTypes, generateTypeNameValue(op.getOpName))))
            List((op.argTypes, generateTypeNameValue(op.getOpName)))
        }
        
        val args:List[Terminal] = List[Terminal]()
        //for(terminal<-op.argTypes.args)
      }

      
      def generateRandomIntLiteral():TypeLiteral= 
      {
        TypeLiteral(scala.util.Random.nextInt.toString)
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
