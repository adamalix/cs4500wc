package com.cpb.cs4500.valueGeneration {
	import org.scalatest.FunSuite
	import com.cpb.cs4500.parsing._
	import com.cpb.cs4500.valueGeneration._
	import scala.collection.mutable.HashMap
	import scala.collection.immutable.ListSet
	
	  
   class TestGeneration extends FunSuite {
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
		val otherCat:OperationSpec = new OperationSpec(makeCat, addArgTypes, catType, true)
		val emptyCat:OperationSpec = new OperationSpec(makeCat, emptyArgs, catType, true)
		val emptyCat2:OperationSpec = new OperationSpec(makeCat2, emptyArgs, catType, true)
		val notBaseCat:OperationSpec = new OperationSpec(makeDog, simpleArgTypes, dogType, false)
		
		val catADTOperationSpecs:OperationSpecs = OperationSpecs(List(baseCat, otherCat, notBaseCat))
		val sig:ADTSignature = ADTSignature(catType, catADTOperationSpecs)
		
		val sigs:ADTSignatures = ADTSignatures(List(sig))
		
		val testSpec:Spec = Spec(sigs, null)
		
		val valueGen:ValueGenerator = new ValueGenerator(testSpec)
		
		// Correct values
		val emptyCatExpr:TermExpr = new TermExpr(makeCat, new EmptyArg())
		
		var emptyCatList:ListSet[Term] = ListSet[Term]()
		
		emptyCatList = emptyCatList + emptyCatExpr

		valueGen.createBasicCreatorMap()
		
		test("test createTestsforOp1") {
			expect(emptyCatList) { valueGen.createTestForOp(makeCat, notBaseCat.getArgTypes()) }
		}
		test("testCreateTests3") {
			expect(emptyCatList) { valueGen.createTestsForOpSpec(emptyCat, 1) }
		}
		
   }
}