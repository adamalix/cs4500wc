package com.cpb.cs4500.valueGeneration {
  import org.scalatest.FunSuite
  import com.cpb.cs4500.io.ReadWriter
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._

  import scala.collection.immutable.ListSet

  class TestGeneration extends FunSuite {
  
    val parser = new ADTParser()

    val testFileName1 = "src/test/resources/test1"
    val testFileName2 = "src/test/resources/test2"
    val testFileName3 = "src/test/resources/test3"
    val testFileName4 = "src/test/resources/test4"
    val testFileName5 = "src/test/resources/test5"
    val testFileName6 = "src/test/resources/test6"
    val testFileName7 = "src/test/resources/test7"

    val testFile1 = ReadWriter.inputFromFile(testFileName1)
    val testFile2 = ReadWriter.inputFromFile(testFileName2)
    val testFile3 = ReadWriter.inputFromFile(testFileName3)
    val testFile4 = ReadWriter.inputFromFile(testFileName4)
    val testFile5 = ReadWriter.inputFromFile(testFileName5)
    val testFile6 = ReadWriter.inputFromFile(testFileName6)
    val testFile7 = ReadWriter.inputFromFile(testFileName7)

    val intLit:IntLiteral = new IntLiteral("3")
    val boolLit:BooleanLiteral = new BooleanLiteral("true")
    val charLit:CharLiteral = new CharLiteral("#c")
    val stringLit:StringLiteral = new StringLiteral("HURRDURR")

    val catType:TypeName = new TypeName("Cat")
    val dogType:TypeName = new TypeName("Dog")

    val addOp:Operation = new Operation("addCat")
    val makeCat:Operation = new Operation("makeCat")
    val makeCat2:Operation = new Operation("makeCat-2")
    val makeDog:Operation = new Operation("makeDog")
    val addBasicTypes:ArgTypes = ArgTypes(List(intLit))
    val addArgTypes:ArgTypes = ArgTypes(List(catType, boolLit, intLit))
    val simpleArgTypes:ArgTypes = ArgTypes(List(catType))
    val emptyList:List[Terminal] = List[Terminal]()
    val emptyArgs:ArgTypes = ArgTypes(emptyList)

    val baseCat:OperationSpec = new OperationSpec(makeCat, addBasicTypes, catType, true)
    val baseCat2:OperationSpec = new OperationSpec(makeCat2, addBasicTypes, catType, true)
    val otherCat:OperationSpec = new OperationSpec(makeCat, addArgTypes, catType, true)
    val emptyCat:OperationSpec = new OperationSpec(makeCat, emptyArgs, catType, true)
    val emptyCat2:OperationSpec = new OperationSpec(makeCat2, emptyArgs, catType, true)
    val notBaseCat:OperationSpec = new OperationSpec(makeDog, emptyArgs, dogType, true)

    val catADTOperationSpecs:OperationSpecs = OperationSpecs(List(emptyCat, emptyCat2, notBaseCat))
    val sig:ADTSignature = ADTSignature(catType, catADTOperationSpecs)

    val sigs:ADTSignatures = ADTSignatures(List(sig))

    val testSpec:Spec = Spec(sigs, null)

    val valueGen:ValueGenerator = new ValueGenerator(testSpec)
      /*  
    test("testBasicCreator") {
      expect("HNG") { valueGen.makeListOfArgs(addArgTypes.args) }
    }

    // Correct values
    val emptyCatExpr:TermExpr = new TermExpr(makeCat, new EmptyArg())

    var emptyCatList:List[Term] = List[Term]()

    emptyCatList = emptyCatExpr :: emptyCatList

    valueGen.createBasicCreatorMap()
    /*
    test("test createTestsforOp1") {
      expect(emptyCatList) { valueGen.createTestForOp(makeCat, notBaseCat.getArgTypes()) }
    }*/

  
    test("testCreateTests3") {
      expect(emptyCatList) { valueGen.makeListOfArgs() }
    }

    test("Test test1 Value Generation") {
      println("********Printing test3 ValGen data: ***********")
      val spec3 = parser.parseAll(parser.spec, testFile1).get
      val gen = new ValueGenerator(spec3)
      val constructorMap = gen.constructorMap
      val basicMap = gen.basicCreatorMap
      //val primopMap = gen.primopMap
      val emptyTest = TermExpr(Operation("empty"), EmptyArg())
      println(spec3)
      println(constructorMap)
      println(basicMap)
      //println(primopMap)
      // we know that there is only one basic creator
      val emptyOpSpec: OperationSpec = basicMap(TypeName("StackInt"))(0)
      expect(emptyTest) {
        gen.createTestForOp(emptyOpSpec.op, emptyOpSpec.getArgTypes)
      }
      val creatorList = constructorMap(TypeName("StackInt"))

      expect(true) {
        val pushSpec = creatorList(1)
        gen.createTestForOp(pushSpec.op, pushSpec.getArgTypes) match {
          case termExpr: TermExpr => {
            println(termExpr)
            println(gen.termIdMap)
            true
          }
          case _ => false
        }
      }
    }
*/
  }
}
