# Used to build the software, invoke by:
# cd <project-base-dir> <-- this is the same directory that this file lives in

all:
	scalac -classpath /usr/share/java/scala-library.jar \
	-sourcepath src -d build src/main/scala/com/cpb/cs4500/Runner.scala \
	src/main/scala/com/cpb/cs4500/parsing/Spec.scala \
	src/main/scala/com/cpb/cs4500/parsing/ADTParser.scala \
	src/main/scala/com/cpb/cs4500/io/ReadWriter.scala \
	src/main/scala/com/cpb/cs4500/rewriting/Rewriter.scala
	cd build && jar -cfm ../bin/cs4500.jar ../src/main/resources/MANIFEST.MF *
	cd ..
