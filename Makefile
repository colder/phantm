all: cup jflex javafiles scalafiles

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
	scalac -d classes `find src -name "*.scala"`

test: scalafiles
	scala -verbose -classpath classes phpanalysis.Main tests/*.php
