package com.cpb.cs4500.util {
  import com.cpb.cs4500.parsing._
  import com.cpb.cs4500.io.ReadWriter

  import org.scalatest.FunSuite

  class TestSchTestConverter extends FunSuite {

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

    // even though this test passes, it's not exhaustive and I know it is still
    // broken. email me if you need examples or have questions.
    test("test findSchConverter") {
      val spec5 = parser.parseAll(parser.spec, testFile5).get
      val opspecsListSet = spec5.getAllOpSpecs
      val opspecs = opspecsListSet.toList
      val zeroSpec = opspecs.last
      expect("(= ") {
        SchTestConverter.findSchConverter(zeroSpec.returnType, opspecsListSet)
      }
    }

  }
}
