package com.cpb.cs4500.io 
{
    import scala.io._
    object fileInputOutput
    {
      def inputFromFile(fileName: String)
      { 
        val s = Source.fromFile(fileName)
        s.getLines.foreach( (line) => {println(line.trim.toUpperCase)})
      }
      def outputToFile(fileName: String, fileContent: String)
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