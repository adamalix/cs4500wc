package com.cpb.cs4500.io 
{
    import scala.io._
    object ReadWriter
    {
      def inputFromFile(fileName: String):String = 
      { 
      
        var str:String = ""
        for (line <- Source.fromFile(fileName).getLines)
            str+= " " + line + " "
        str 
      }
      def outputToFile(fileName: String, fileContent: String):Unit = 
      {
        val out = new java.io.FileWriter(fileName)
        out.write(fileContent)
        out.close
      }
    }
    
}