/* 
ReadWriter is a static class (an object in scala) that
can input from a file or output to one.
*/

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

        var x:Int = 0;
        while(x < fileContent.length) {
            out.write(fileContent.substring(x, x+1))
            x+= 1
        }
        out.close
      }
    }  
}