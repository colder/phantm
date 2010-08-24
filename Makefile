all: warning

warning:
	@ echo "THIS MAKEFILE IS ONLY HERE TO BUILD CUP/JFLEX"
	@ echo "To (re)compile your scala code, use 'sbt'"
	@ echo "If you really want to (re)generate cup/jflex, run 'make bootstrap'"


bootstrap: setup build_cup cup jflex

setup:
	@ test -d classes || mkdir classes
	@ test -d lib/cup/lib || mkdir lib/cup/lib

build_cup:
	cd lib/cup && ant

cup:
	java -jar lib/cup/dist/java-cup-11a.jar -parser CUPParser -package phantm.parser -destdir src/main/java/phantm/parser/ -files -symbols Symbols spec/php.cup

jflex:
	java -jar bin/JFlex.jar -d src/main/java/phantm/parser/ -nobak spec/php.jflex
