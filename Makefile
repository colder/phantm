partial: scalafiles

all: setup cup jflex javafiles scalafiles

complete: build_cup all

setup:
	@ test -d classes || mkdir classes
build_cup:
	cd lib/cup && ant

clean:
	find java/phpanalysis/parser/ -type f -iname "*.java" -exec rm '{}' \;

cup:
	java -jar lib/cup/dist/java-cup-11a.jar -parser Parser -package phpanalysis.parser -destdir java/phpanalysis/parser/ -files -symbols Symbols spec/php.cup

jflex:
	java -jar bin/JFlex.jar -d java/phpanalysis/parser/ -nobak spec/php.jflex

javafiles:
	javac -cp lib/cup/dist/java-cup-11a-runtime.jar -d classes/ `find java -name "*.java"`

scalafiles:
	fsc -classpath lib/cup/dist/java-cup-11a-runtime.jar -unchecked -deprecation -d classes `find src -name "*.scala"`

test:
	scala -verbose -classpath classes phpanalysis.Main tests/*.php
