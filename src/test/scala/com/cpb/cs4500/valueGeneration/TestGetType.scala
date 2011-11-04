/*
TestParse.scala
Holds the test cases for the parsing mechanism of our software
*/
package com.cpb.cs4500.parsing {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.valueGeneration._
  import scala.collection.mutable.HashMap
  import scala.collection.immutable._

  class TestGetType extends FunSuite {
        
    val intType = TypeLiteral("int")
    val booleanType = TypeLiteral("boolean")
    val stringType = TypeLiteral("string")
    val charType = TypeLiteral("character")
    val car = Operation("Car")
    val makeBoolOp = Operation("makeBool")
    val makeBoolArgTypes = ArgTypes(List(intType, intType))
    val makeBoolOpSpec = OperationSpec(makeBoolOp, makeBoolArgTypes, booleanType, false)
    val makeCarOpSpec = OperationSpec(car, makeBoolArgTypes, intType, false)
    val thisADTOperationSpecs = OperationSpecs(List(makeBoolOpSpec, makeCarOpSpec))
    val adtName = "THISADT"
    val thisADT = TypeName(adtName) 
    val thisADTSignature = ADTSignature(thisADT, thisADTOperationSpecs)
    val testADTSignatureList = ADTSignatures(List(thisADTSignature))
    val term = Term("", Operation(""), Arg())
    val testSpecEquations = Equations(List(Equation(term, term)))
    val emptyEquations = Equations(List())
    //val testSpec = Spec(testADTSignatureList, emptyEquations)
    val valMap:HashMap[OperationSpec, List[((ArgTypes, TypeLiteral))]] = new HashMap()
    
    val catType = TypeName("Cat")
    val makeArgs = ArgTypes(List[Terminal]())
    val makeCatName = Operation("emptyCat")
    val makeCat = OperationSpec(makeCatName, makeArgs, catType, true)
    val typeMap:HashMap[TypeName, OperationSpec] = new HashMap()
    typeMap.put(catType, makeCat)
    val makeBoolArgTypes2 = ArgTypes(List(intType, catType))
    val makeBoolOpSpec2 = OperationSpec(makeBoolOp, makeBoolArgTypes2, catType, false)

    val newType = TypeName("NewType")
    val newTypeMethodOp = Operation("newTypeMethod")
    val newTypeMethodArgs = ArgTypes(List(charType, stringType))
    val newTypeMethodOpSpec = OperationSpec(newTypeMethodOp, newTypeMethodArgs, stringType, false)
    //System.out.println(ValueGenerator.opSpecReplacement(makeCat, List[Terminal]() , valMap, typeMap).mkString)


    
    /*
    test("testGetTypes") {
        expect (" ") {testSpec.getAllOpNames()}}
    */
  }

}
