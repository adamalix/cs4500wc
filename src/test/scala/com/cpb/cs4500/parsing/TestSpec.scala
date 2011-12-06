package com.cpb.cs4500.parsing {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.io.ReadWriter
  import com.cpb.cs4500.parsing._
    import scala.collection.mutable.HashMap
  import scala.collection.immutable.ListSet
  
   class TestSpec extends FunSuite {
   
		val intLit:IntLiteral = new IntLiteral("3")
		val boolLit:BooleanLiteral = new BooleanLiteral("true")
		val charLit:CharLiteral = new CharLiteral("#c")
		val stringLit:StringLiteral = new StringLiteral("HURRDURR")
		
		val catType:TypeName = new TypeName("Cat")
		val dogType:TypeName = new TypeName("Dog")
		
		val addOp:Operation = new Operation("addCat")
		val makeCat:Operation = new Operation("makeCat")
		val makeDog:Operation = new Operation("makeDog")
		val addArgTypes:ArgTypes = ArgTypes(List(catType, dogType, intLit))
		val emptyList:List[Terminal] = List[Terminal]()
		val emptyArgs:ArgTypes = ArgTypes(emptyList)
		
		val baseCat:OperationSpec = new OperationSpec(makeCat, emptyArgs, catType, true)
		val otherCat:OperationSpec = new OperationSpec(makeCat, emptyArgs, catType, true)
		val notBaseCat:OperationSpec = new OperationSpec(makeDog, addArgTypes, dogType, false)
		
		val catADTOperationSpecs:OperationSpecs = OperationSpecs(List(baseCat, otherCat, notBaseCat))
		val sig:ADTSignature = ADTSignature(catType, catADTOperationSpecs)
		
		val sigs:ADTSignatures = ADTSignatures(List(sig))
		
		val testSpec:Spec = Spec(sigs, null)
		
		var typeNameSetList:ListSet[TypeName] = ListSet[TypeName]()
		typeNameSetList = typeNameSetList + catType + dogType
		
		var typeLitSetList:ListSet[TypeLiteral] = ListSet[TypeLiteral]()
		typeLitSetList = typeLitSetList + intLit 	
		
		var argsList:List[Terminal] = List[Terminal]()
		argsList = argsList :+ catType :+ dogType :+ intLit
		
		var allConst:ListSet[OperationSpec] = ListSet[OperationSpec]()
		allConst = allConst + baseCat + otherCat
		
		var dogConst:ListSet[OperationSpec] = ListSet[OperationSpec]()
		dogConst = dogConst + notBaseCat
		
		var constMap:HashMap[TypeName, ListSet[OperationSpec]] = HashMap[TypeName, ListSet[OperationSpec]]()
		constMap.put(catType, allConst)
		constMap.put(dogType, dogConst)

		test("testGetTypeNames") 
		{
			expect(typeNameSetList) { testSpec.getAllTypeNames() }
		}
		
		test("testGetTypeLits")
		{
			expect(typeLitSetList) { testSpec.getAllTypeLiterals() }
		}
		
		test("testGetArgs")
		{
			expect(argsList) { notBaseCat.getArgTypes() }
		}
		
		test("getAllConstructors")
		{
			expect (constMap) { testSpec.getAllConstructors() }
		}
		
		test("getAllOperationNames")
		{
			expect(List("makeDog", "makeCat")) { testSpec.getOperationNames } 
		}
		
	}
}