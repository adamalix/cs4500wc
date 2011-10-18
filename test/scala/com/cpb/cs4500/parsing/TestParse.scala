import java.io.FileReader
import com.cpb.cs4500.parsing.ADTParser

object TestParse extends ADTParser {
  def main(args: Array[String]) {
    val thingy = "Signatures: ADT: THISADT makeBool: int * int -> boolean Equations:"
//    val reader = new FileReader(args(0))
    println(parseAll(spec, thingy))
  }

}
