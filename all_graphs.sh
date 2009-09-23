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
    echo "Graph saved to http://project.colder.ch/$NAME-AST.jpg"
    dot -Tjpg -o ${DIST_PATH}${NAME}-ST.jpg result.st && mv result.st ${DIST_PATH}${NAME}-ST.jpg.txt
    echo "Graph saved to http://project.colder.ch/$NAME-ST.jpg"
    for f in result.cfg-*; do
        if [ $f != 'result.cfg-*' ]; then
            N=`echo $f | cut -d'-' -f2`
            dot -Tjpg -o ${DIST_PATH}${NAME}-CFG${N}.jpg $f && mv $f ${DIST_PATH}${NAME}-CFG${N}.jpg.txt
            echo "Graph saved to http://project.colder.ch/$NAME-CFG${N}.jpg"
        fi
    done
    #/dot -Tjpg -o ${DIST_PATH}${NAME}-CFG.jpg result.cfg && cp result.cfg ${DIST_PATH}${NAME}-CFG.jpg.txt

    cp $1 ${DIST_PATH}${NAME}s
fi
