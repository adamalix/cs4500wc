package com.cpb.cs4500.io 
{
    import scala.io._
    object ReadWriter
    {
      def inputFromFile(fileName: String):String = 
      { 
        val s = Source.fromFile(fileName)
        val str:String = ""
        s.getLines.foreach((line)=> {str+line})
        str 
      }
      def outputToFile(fileName: String, fileContent: String):Unit = 
      {
        val out = new java.io.FileWriter(fileName)
        out.write(fileContent)
        out.close
      }
      
      def durp()
      {
        println("HEYEHYEYEHEY")
      }
    }
    
}