package com.cpb.cs4500.valueGeneration {
    import com.cpb.cs4500.parsing._
    import scala.collection.mutable.HashMap
    import scala.collection.immutable._
    object ValueGenerator{
      
      // Our value map. The Key is the Type and the Value is
      // a list of Strings (So we can have more than one value).
      val valueMap = new HashMap[String, String]
      
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
    
    }
}
