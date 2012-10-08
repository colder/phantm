all: warning

warning:
	@ echo "THIS MAKEFILE IS ONLY HERE TO BUILD CUP/JFLEX"
	@ echo "If you really want to (re)generate cup/jflex, run 'make bootstrap'"
	@ echo "To (re)compile scala/java code, use 'sbt package'"


bootstrap: setup build_cup cup jflex bootstrapjar

setup:
	@ test -d classes || mkdir classes
	@ test -d lib/cup/lib || mkdir lib/cup/lib

build_cup:
	cd lib/cup && ant

cup:
	java -jar lib/cup/dist/java-cup-11a.jar -parser CUPParser -package phantm.parser -destdir src/main/java/phantm/parser/ -files -symbols Symbols spec/php.cup

jflex:
	java -jar bin/JFlex.jar -d src/main/java/phantm/parser/ -nobak spec/php.jflex

bootstrapjar:
	@ mkdir -p tmp/spec tmp/tables
	@ echo -n "<build><version>"                           > tmp/build.xml
	@ grep -o 'version := ".\+"' build.sbt | cut -d'"' -f2 >> tmp/build.xml
	@ echo -n "</version><date>"                           >> tmp/build.xml
	@ date                                                 >> tmp/build.xml
	@ echo -n "</date></build>"                            >> tmp/build.xml
	@ cp spec/internal_api.xml tmp/spec
	@ cp src/main/java/phantm/parser/action_table.bin      tmp/tables
	@ cp src/main/java/phantm/parser/production_table.bin  tmp/tables
	@ cp src/main/java/phantm/parser/reduce_table.bin      tmp/tables
	jar cf lib/phantm-files.jar -C tmp build.xml -C tmp spec -C tmp tables
	@ rm -rf tmp

release:
	java -jar bin/proguard.jar @proguard.conf
