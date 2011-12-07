package com.cpb.cs4500.valueGeneration {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.rewriting.Rewriter
  import scala.collection.mutable.Map
  import scala.collection.mutable.HashSet
  import scala.collection.immutable.ListSet

  class ValueGenerator(specification: Spec) {

    // Identify basic creators for convenience
    var typeMap: Map[TypeName, List[Term]] = createBasicCreatorMap()

    // entry point, create all tests for a spec
    def createAllTests(depth: Int): List[Term] = {
      var allTests: List[Term] = List[Term]()
      val allOpSpecs: ListSet[OperationSpec] = specification.getAllOpSpecs()
      var currentDepth: Int = 0
      while (currentDepth <= depth){
        for (opSpec <- allOpSpecs) {
          val testsForOpSpec: List[Term] = createTests(opSpec)
          allTests = allTests ++ testsForOpSpec
        }
        currentDepth += 1
      }
      allTests
    }
    
    def createTests(opspec: OperationSpec) : List[Term] = {
      val args: List[Terminal] = opspec.argTypes.args
      var generatedArgs: List[List[Term]] = makeListOfArgs(args)
      var tests: List[Term] = List[Term]()
      for (termArgs <- generatedArgs) {
        var termExpr: TermExpr = new TermExpr(opspec.op, convertListToArgs(termArgs))
        tests = tests :+ termExpr
      }
      tests
    }
    
    def convertListToArgs(list: List[Term]): Arg = {
      var ArgObj: Arg = new EmptyArg()
      if (list.isEmpty) {
        new EmptyArg()
      }
      else {
        for (term <- list.reverse) {
          ArgObj = new Args(term, ArgObj)  
        }
        ArgObj
      }
    
    }
    
    // Cartesian products of the arguments (all possible arguments)
    def makeListOfArgs(args: List[Terminal]): List[List[Term]] = {
      var allTermList: List[List[Term]] = List[List[Term]]()
      for (arg <- args) {
        arg match {
          case name: TypeName => allTermList = allTermList :+ typeMap(name)
          case intLiteral: TypeLiteral => allTermList = allTermList :+ List[Term](new TermID(generateRandomInt().toString()))
        }
      }
      cart[Term](allTermList)
    }
    // Written by: http://anders.janmyr.com/2009/10/lists-in-scala.html
    def cart[T](listOfLists: List[List[T]]): List[List[T]] = listOfLists match {
      case Nil => List(List())
      case xs :: xss => for (y <- xs; ys <- cart(xss)) yield y :: ys
    }
    
    
    // Store basic creators for convenient use later. A basic creator
    // is defined as an OperationSpec who's returnType returns a TypeName
    // and it has no TypeName's in its arguments
    def createBasicCreatorMap(): Map[TypeName, List[Term]] = {
      val bcMap = Map[TypeName, List[Term]]()
      val basicCreators: ListSet[OperationSpec] = specification.getAllBaseConstructors()
      for (basicCreator <- basicCreators){
        val arguments: List[Terminal] = basicCreator.argTypes.args
        var termArguments: Arg = new EmptyArg()
        
        // TODO Make so that primitive arguments are generated.
        for (arg <- arguments){
          var arg: TermID = new TermID(generateRandomInt().toString())
          termArguments = new Args(arg, termArguments)
        }
        
        basicCreator.returnType match {
          case rt: TypeName => {
            var createdTerm: Term = new TermExpr(basicCreator.op, termArguments)
            var termList: List[Term] = List[Term]()
            if(bcMap.contains(rt))  {
              termList = bcMap(rt) ++ List[Term](createdTerm)
            }
            else{
              termList = List[Term](createdTerm)
            }
            bcMap += (rt -> termList)
          }
        }
      }
      bcMap
    }

          
    // Creates a random intLiteral between 0-9
    def generateRandomInt(): IntLiteral = {
      new IntLiteral(scala.util.Random.nextInt(10))
    }

    // Creates a random BooleanLiteral
    def generateRandomBoolean(): BooleanLiteral = {
      new BooleanLiteral(scala.util.Random.nextBoolean)
    }

    // Creates a random CharLiteral
    def generateRandomChar(): CharLiteral = {
      new CharLiteral(scala.util.Random.nextPrintableChar)
    }

    // Creates a random String literal from this list.
    def generateRandomString(): StringLiteral = {
      val strList = List("string1", "string2", "string3", "string4", "string5")
      new StringLiteral(strList(scala.util.Random.nextInt(5)))
    }

    /*
    def generateRandomTermID(): TermID = {
      var term = TermID(scala.util.Random.nextString(12))
      // check the set for the term
      while (generatedTermIds.contains(term)) {
        term = TermID(scala.util.Random.nextString(12))
      }
      generatedTermIds += term
      term
    }
    */

  }

}
