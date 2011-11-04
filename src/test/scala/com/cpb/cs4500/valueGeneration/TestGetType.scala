/*
TestParse.scala
Holds the test cases for the parsing mechanism of our software
*/
package com.cpb.cs4500.parsing {

  import org.scalatest.FunSuite
  import com.cpb.cs4500.parsing._

  class TestGetType extends FunSuite {
        
    val intType = TypeLiteral("int")
    val booleanType = TypeLiteral("boolean")
    val car = Operation("Car")
    val makeBoolOp = Operation("makeBool")
    val makeBoolArgTypes = ArgTypes(List(intType, intType))
    val makeBoolOpSpec = OperationSpec(makeBoolOp, makeBoolArgTypes, booleanType)
    val makeCarOpSpec = OperationSpec(car, makeBoolArgTypes, intType)
    val thisADTOperationSpecs = OperationSpecs(List(makeBoolOpSpec, makeCarOpSpec))
    val adtName = "THISADT"
    val thisADT = TypeName(adtName) 
    val thisADTSignature = ADTSignature(thisADT, thisADTOperationSpecs)
    val testADTSignatureList = ADTSignatures(List(thisADTSignature))
    val term = Term("", Operation(""), List())
    val testSpecEquations = Equations(List(Equation(term, term)))
    val emptyEquations = Equations(List())
    val testSpec = Spec(testADTSignatureList, emptyEquations)
    
    System.out.println(testSpec.toString)
    
    /*
    test("testGetTypes") {
        expect (" ") {testSpec.getAllOpNames()}}
    */
  }

}
