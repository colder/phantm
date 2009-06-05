#!/bin/sh
if [ $# -lt 1 ]; then
    echo "Usage: $0 <file>"
    exit 1;
fi
./helpers.sh -t ST -o result.st $1
./helpers.sh -t AST -o result.ast $1
./helpers.sh -t CFG -o result.cfg $1

DIST_PATH=/home/wwwrun/colder.ch/project/
NAME=`basename $1`
if [ -f "result.st" ]; then
    dot -Tjpg -o ${DIST_PATH}${NAME}-AST.jpg result.ast && mv result.ast ${DIST_PATH}${NAME}-AST.jpg.txt
    dot -Tjpg -o ${DIST_PATH}${NAME}-ST.jpg result.st && mv result.st ${DIST_PATH}${NAME}-ST.jpg.txt
    dot -Tjpg -o ${DIST_PATH}${NAME}-CFG.jpg result.cfg && mv result.cfg ${DIST_PATH}${NAME}-CFG.jpg.txt

    cp $1 ${DIST_PATH}${NAME}s
    echo "Graph saved to $JPG_PATH (http://project.colder.ch/$NAME-AST.jpg)"
    echo "Graph saved to $JPG_PATH (http://project.colder.ch/$NAME-ST.jpg)"
    echo "Graph saved to $JPG_PATH (http://project.colder.ch/$NAME-CFG.jpg)"
fi
