# Used to build the software, invoke by:
# cd <project-base-dir> <-- this is the same directory that this file lives in

all:
	rm -rf build && mkdir build
	/course/cs4500wc/bin/scalac -deprecation -classpath /course/cs4500wc/lib/scala-library.jar \
	-sourcepath src -d build src/main/scala/com/cpb/cs4500/Runner.scala \
	src/main/scala/com/cpb/cs4500/io/ReadWriter.scala \
	src/main/scala/com/cpb/cs4500/parsing/ADTParser.scala \
	src/main/scala/com/cpb/cs4500/parsing/Spec.scala \
	src/main/scala/com/cpb/cs4500/rewriting/Rewriter.scala \
	src/main/scala/com/cpb/cs4500/util/InfiniteRewriteException.scala \
	src/main/scala/com/cpb/cs4500/util/SchTestConverter.scala \
	src/main/scala/com/cpb/cs4500/valueGeneration/ValueGenerator.scala
	cd build && jar -cfm ../bin/cs4500.jar ../software_update/MANIFEST.MF *
	cd ..
