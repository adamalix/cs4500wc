package com.cpb.cs4500.valueGeneration {
    import com.cpb.cs4500.parsing._

    trait GeneratedValue{
    }
    
    class GeneratedPrimitive(lit:TypeLiteral) extends GeneratedValue{
        def getLiteral:TypeLiteral = 
        {
            lit
        }
    }
    
    class GeneratedFunction(name:String, arguments:List[GeneratedValue], returnType:Terminal) extends GeneratedValue{
    }

}