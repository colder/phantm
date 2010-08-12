partial: scalafiles

all: setup cup jflex javafiles rebuild

complete: setup build_cup all

setup:
	@ test -d classes || mkdir classes
	@ test -d lib/cup/lib || mkdir lib/cup/lib

build_cup:
	cd lib/cup && ant

clean-all:
	find java/phantm/parser/ -type f -iname "*.java" -exec rm '{}' \;

clean:
	find classes -type f -iname "*.class" -exec rm '{}' \;

touch-scala:
	find src/phantm/ -type f -iname "*.scala" -exec touch '{}' \;

rebuild:
	@ant build_complete
	@ant jar

cup:
	java -jar lib/cup/dist/java-cup-11a.jar -parser CUPParser -package phantm.parser -destdir java/phantm/parser/ -files -symbols Symbols spec/php.cup

jflex:
	java -jar bin/JFlex.jar -d java/phantm/parser/ -nobak spec/php.jflex

javafiles:
	javac -cp lib/cup/dist/java-cup-11a-runtime.jar -d classes/ `find java -name "*.java"`

scalafiles:
	@ ant jar

test:
	scala -verbose -classpath classes phantm.Main tests/*.php
