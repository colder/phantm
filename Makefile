partial: scalafiles

all: setup cup jflex javafiles scalafiles

complete: setup build_cup all

setup:
	@ test -d classes || mkdir classes
	@ test -d lib/cup/lib || mkdir classes

build_cup:
	cd lib/cup && ant

clean-all:
	find java/phpanalysis/parser/ -type f -iname "*.java" -exec rm '{}' \;

clean:
	find classes -type f -iname "*.class" -exec rm '{}' \;

touch-scala:
	find src/phpanalysis/ -type f -iname "*.scala" -exec touch '{}' \;

clean-build: touch-scala scalafiles

cup:
	java -jar lib/cup/dist/java-cup-11a.jar -parser Parser -package phpanalysis.parser -destdir java/phpanalysis/parser/ -files -symbols Symbols spec/php.cup

jflex:
	java -jar bin/JFlex.jar -d java/phpanalysis/parser/ -nobak spec/php.jflex

javafiles:
	javac -cp lib/cup/dist/java-cup-11a-runtime.jar -d classes/ `find java -name "*.java"`

scalafiles:
	@ ant

test:
	scala -verbose -classpath classes phpanalysis.Main tests/*.php
