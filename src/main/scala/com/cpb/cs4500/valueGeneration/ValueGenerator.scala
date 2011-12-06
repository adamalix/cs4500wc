package com.cpb.cs4500.valueGeneration {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.rewriting.Rewriter
  import scala.collection.mutable.Map
  import scala.collection.mutable.HashSet
  import scala.collection.immutable.ListSet

  class ValueGenerator(specification: Spec) {

    // Initialize a map containing TypeNames and Lists of their constructors
    val constructorMap: Map[TypeName, List[OperationSpec]] = createConstructorMap()

    // Identify basic creators for convenience
    val basicCreatorMap: Map[TypeName, List[OperationSpec]] = createBasicCreatorMap()

    //val primopMap: Map[TypeName, List[OperationSpec]] = createPrimopMap()

    // Store the generated TypeLiterals here for replacement when rewriting
    val termIdMap = Map[TermID, TypeLiteral]()

    // entry point, create all tests for a spec
    def createAllTests(minTestsForSpec: Int): List[List[Term]] = {
      var tests = List[List[Term]]()
      for (opspec <- specification.getAllOpSpecs)
        tests = createTestsForOpSpec(opspec, minTestsForSpec) :: tests

      tests
    }

    // Keep track of all generated TermIDs between test generation sessions so
    // we don't get duplicate data
    val generatedTermIds = HashSet[TermID]()
/*
    def createPrimopMap(): Map[TypeName, List[OperationSpec]] = {
      val opspecList: ListSet[OperationSpec] = specification.getAllOpSpecs()
      val destMap = Map[TypeName, List[OperationSpec]]()

      for (opspec <- opspecList) {
        var opspecListDest = List[OperationSpec]()
        if (!opspec.getArgTypes.contains(opspec.returnType))
          opspecListDest = opspec :: opspecListDest
        destMap += (typeName -> opspecListDest)
      }
      destMap
    }
*/
    // Pull all constructors out of the specification
    def createConstructorMap(): Map[TypeName, List[OperationSpec]] = {
      val constructorsMap: Map[TypeName, ListSet[OperationSpec]] = specification.getAllConstructors()
      // Temp destination map because we parse into ListSets
      val destMap = Map[TypeName, List[OperationSpec]]()

      // Key, Value
      for ((typeName, opspecList) <- constructorsMap) {
        var opspecListDest = List[OperationSpec]()
        opspecList.foreach((op: OperationSpec) => opspecListDest = op :: opspecListDest)
        destMap += (typeName -> opspecListDest)
      }
      destMap
    }

    // Store basic creators for convenient use later
    def createBasicCreatorMap(): Map[TypeName, List[OperationSpec]] = {
      val bcMap = Map[TypeName, List[OperationSpec]]()

      // Key, Value
      for ((typeName, opspecList) <- constructorMap) {
        var basicCreatorList = List[OperationSpec]()

        for (opspec <- opspecList)
          if (opspec.isBasicCreator()) basicCreatorList = opspec :: basicCreatorList

        bcMap += (typeName -> basicCreatorList)
      }

      bcMap
    }

    // create a term for this opspec
    def createTestsForOpSpec(opspec: OperationSpec, numTests: Int): List[Term] = {
      val argTypes: List[Terminal] = opspec.getArgTypes

      // bottom out immediately if we have no argTypes
      if (argTypes.isEmpty)
          return List(TermExpr(opspec.op, EmptyArg()))

      // Place to uniquely store tests
      var tests = List[Term]()

      while (tests.length < numTests) {
        val test = createTestForOp(opspec.op, argTypes)
        if (!tests.contains(test))
          tests = test :: tests
      }

      tests
    }

    // Create a single TermExpr for an Operation
    // Individual test generation
    def createTestForOp(op: Operation, argTypes: List[Terminal]): Term = {
      if (argTypes.isEmpty)
        return TermExpr(op, EmptyArg())

      TermExpr(op, createArgs(argTypes))
    }

    // Recursively create args for the TermExpr
    def createArgs(argTypes: List[Terminal]): Arg = {
      // Base Case
      if (argTypes.isEmpty)
        return EmptyArg()

      // Do we have a TypeLiteral or TypeName?
      val term = argTypes.head match {
        case literal: TypeLiteral => createTermAndValueForTypeLit(literal)
        case typeName: TypeName => createTermForTypeName(typeName)
      }

      Args(term, createArgs(argTypes.tail))
    }

    // Create the term for a TypeName by using a basic creator
    def createTermForTypeName(typeName: TypeName): Term = {
      // get the list of basic creators for the TypeName
      val basicCreators: List[OperationSpec] = basicCreatorMap(typeName)

      // wtf happens here
      if (basicCreators.isEmpty)
        throw new RuntimeException("Cannot instantiate ADT: " + typeName +
                                   " because it has no basic creator")

      // TODO: DONT DO THIS IN THE FUTURE
      // we should be choosing correct basic or attempting to guess at it
      val idx = scala.util.Random.nextInt(basicCreators.size)
      val randomCreator = basicCreators(idx)
      createTestForOp(randomCreator.op, randomCreator.getArgTypes())
    }

    def createTermAndValueForTypeLit(typeLit: TypeLiteral): Term = {
      // generate a random ID
      val termId: TermID = generateRandomTermID()

      // generate random value, map it to ID
      typeLit match {
        case intLit: IntLiteral => {
          termIdMap += (termId -> generateRandomInt())
        }
        case boolLit: BooleanLiteral => {
          termIdMap += (termId -> generateRandomBoolean())
        }
        case charLit: CharLiteral => {
          termIdMap += (termId -> generateRandomChar())
        }
        case stringLit: StringLiteral => {
          termIdMap += (termId -> generateRandomString())
        }
      }

      termId
    }

    def generateRandomInt(): IntLiteral = {
      new IntLiteral(scala.util.Random.nextInt(10))
    }

    def generateRandomBoolean(): BooleanLiteral = {
      new BooleanLiteral(scala.util.Random.nextBoolean)
    }

    def generateRandomChar(): CharLiteral = {
      new CharLiteral(scala.util.Random.nextPrintableChar)
    }

    def generateRandomString(): StringLiteral = {
      val strList = List("string1", "string2", "string3", "string4", "string5")
      new StringLiteral(strList(scala.util.Random.nextInt(5)))
    }

    def generateRandomTermID(): TermID = {
      var term = TermID(scala.util.Random.nextString(12))
      // check the set for the term
      while (generatedTermIds.contains(term)) {
        term = TermID(scala.util.Random.nextString(12))
      }
      generatedTermIds += term
      term
    }

  }

}
