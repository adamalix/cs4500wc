package com.cpb.cs4500.valueGeneration {
  import org.scalatest.FunSuite
  import com.cpb.cs4500.io.ReadWriter
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._

  import scala.collection.immutable.ListSet

  class TestGeneration extends FunSuite {
    val parser = new ADTParser()
    val testFileName = "src/test/resources/test5"
    val testFile = ReadWriter.inputFromFile(testFileName)
    val spec = parser.parseAll(parser.spec, testFile).get

    val valGen = new ValueGenerator(spec)
    val termList = valGen.createAllTests(2)

    /*
    Functions to test:
    - createAllTests
    - createTests
    - convertListToArgs
    - makeListOfArgs
    x createBasicCreatorMap
    x cart
    */
    
    for (term <- termList){
        print(term + "\n")
    }   
    print(termList.size) 

    test("test createAllTests"){
        //this might not get tested, it's a ridiculous output
    }   
    
    test("test makeListOfArgs"){
        val emptyArg = EmptyArg()
        expect(emptyArg){ valGen.convertListToArgs(List()) }

        val termID1 = TermID("lol")
        val termID2 = TermID("lol2")
        val listArgs = List(termID1, termID2)
        val argsArgs = Args(termID1, Args(termID2, emptyArg))
        expect(argsArgs) { valGen.convertListToArgs(listArgs) }
    }

    test("test createBasicCreatorMap"){
        //nat Basic Creator data
        val natTypeName = TypeName("Nat")
        val zeroOp = Operation("zero")
        val natCreator = TermExpr(zeroOp, EmptyArg())
        val testCreatorMap = Map(natTypeName -> List(natCreator))
        expect(testCreatorMap) {
            valGen.createBasicCreatorMap()
        }
    }

    test("test cart"){
        val list1 = List(1, 2)
        val list2 = List(3, 4)
        val list12 = List(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
        val list3 = List(1, 3)
        val list4 = List(2, 4)
        val list34 = List(List(1, 2), List(1, 4), List(3, 2), List(3, 4))

        expect(list12){ valGen.cart(List(list1, list2))}
        expect(list34){ valGen.cart(List(list3, list4))}
    }
  }
}

//express the idea that using this update would result in making the software much more robust, save us work, and requires little effort on the part of the client (is package)
