/**
 * Class used to generate tests for a specification
 * Entry point: createAllTests(test-depth: Int)
 */

package com.cpb.cs4500.valueGeneration {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.rewriting.Rewriter
  import scala.collection.mutable.Map

  class ValueGenerator(specification: Spec) {

    // Identify basic creators for convenience
    var typeMap: Map[TypeName, List[Term]] = createBasicCreatorMap()

    // entry point, create all tests for a spec
    def createAllTests(depth: Int): List[Term] = {
      var allTests: List[Term] = List[Term]()
      val allOpSpecs: List[OperationSpec] = specification.getAllOpSpecs().toList
      var opToType: Map[Operation, TypeName] = makeOpSpecMap(allOpSpecs)
      var currentDepth: Int = 0

      // Continously create tests until we hit the required depth.
      while (currentDepth < depth) {
        for (opSpec <- allOpSpecs) {
          val testsForOpSpec: List[Term] = createTests(opSpec)
          allTests = allTests ++ testsForOpSpec
        }

        // Put the generated values in a map, so they can be used in further depths.
        for (test <- allTests) {
          test match {
            case testExpr:TermExpr => {
              if (opToType.contains(testExpr.op) && !typeMap(opToType(testExpr.op)).contains(test)) {
                typeMap(opToType(testExpr.op)) = typeMap(opToType(testExpr.op)) ++ List[Term](test)
              }
            }
          }
        }
        currentDepth = currentDepth + 1
      }
      allTests
    }

    // Creates a map that given given an Operation, will return the TypeName
    // of its return type. It should be noted that if a operation
    // spec's return Type is a literal we will simply ignore it.
    def makeOpSpecMap(opspecs: List[OperationSpec]): Map[Operation, TypeName] = {
      var opSpecMap = Map[Operation, TypeName]()
      for (opspec <- opspecs) {
        opspec.returnType match {
          case typeN: TypeName => opSpecMap += ( opspec.op -> typeN)
          case _ =>
        }
      }
      opSpecMap
    }

    // Given on function, create all possible tests for it.
    // This function will produce different results based
    // on the current depth.
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


    // Given a List[Term] converts it to an Arg.
    // This is useful in converting from a OperationSpec to a Term
    def convertListToArgs(list: List[Term]): Arg = {
      var ArgObj: Arg = new EmptyArg()
      for (term <- list.reverse) {
        ArgObj = new Args(term, ArgObj)
      }
      ArgObj
    }

    // Cartesian products of the arguments (all possible arguments)
    // This us allows us to generate exhaustive testing based on the depth.
    def makeListOfArgs(args: List[Terminal]): List[List[Term]] = {
      var allTermList: List[List[Term]] = List[List[Term]]()
      for (arg <- args) {
        arg match {
          case name: TypeName => allTermList = allTermList :+ typeMap(name)
          case intLiteral: IntLiteral =>  {
            allTermList = allTermList :+
                          List[Term](new TermID(generateRandomInt().toString()))
          }
          case boolLiteral: BooleanLiteral =>  {
            allTermList = allTermList :+
                          List[Term](new TermID(generateRandomBoolean().toString()))
          }
          case charLiteral: CharLiteral => {
            allTermList = allTermList :+
                          List[Term](new TermID("#\\" +generateRandomChar().toString()))
          }
          case stringLiteral: StringLiteral => {
            allTermList = allTermList :+
                          List[Term](new TermID(generateRandomString().toString()))
          }
        }
      }
      allTermList = optimize(allTermList)
      val fullList: List[List[Term]] = cart[Term](allTermList)
      var reducedList: List[List[Term]] = List[List[Term]]()
      var count = 0
      while (fullList.size > count && count < 100) {
        reducedList = reducedList :+ fullList(count)
        count = count + 1
      }
      reducedList
    }


    // Optimize shrinks a list if the total members of a Cart of said list
    // is greater than 1,000,000. If that is the case optimize will randomly
    // shrink the size of each list (until the size is 25), ensuring that
    // the cartesian product of the list will shrink to a managable size.
    // We choose which values to keep randomly, so there is a loss of some data,
    // but due to the randomness we still ensure that we get quality tests.
    def optimize(list: List[List[Term]]) : List[List[Term]]  = {
      var totalComp = 1;
      var optimizedList = list
      for (l <- list) {
        totalComp = totalComp * l.size
      }
      if (totalComp > 1000000) {
        optimizedList = List[List[Term]]()
        for (individualList <- list) {
          var count = 0
          var redoneList = List[Term]()
          while (count < 25) {
            var randomElement = individualList(scala.util.Random.nextInt(individualList.size))
            while (redoneList.contains(randomElement)) {
              randomElement = individualList(scala.util.Random.nextInt(individualList.size))
            }
            redoneList = redoneList :+ randomElement
            count = count + 1
          }
          optimizedList = optimizedList :+ redoneList
        }
      }
      optimizedList
    }

    // Given a List of Lists, creates the cartesian products for all lists
    // in the list.
    def cart[T](listOfLists: List[List[T]]): List[List[T]] = listOfLists match {
      case Nil => List(List())
      case xs :: xss => for (y <- xs; ys <- cart(xss)) yield y :: ys
    }

    // Store basic creators for convenient use later. A basic creator
    // is defined as an OperationSpec who's returnType returns a TypeName
    // and it has no TypeName's in its arguments
    def createBasicCreatorMap(): Map[TypeName, List[Term]] = {
      val bcMap = Map[TypeName, List[Term]]()
      val basicCreators: List[OperationSpec] = specification.getAllBaseConstructors().toList
      for (basicCreator <- basicCreators) {
        val arguments: List[Terminal] = basicCreator.argTypes.args
        var termArguments: Arg = new EmptyArg()
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

    // Creates a random intLiteral between 0-25
    def generateRandomInt(): IntLiteral = {
      new IntLiteral(scala.util.Random.nextInt(26))
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

  }
}
