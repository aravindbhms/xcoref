#!/bin/bash
#PBS -l mem=230gb,nodes=1:ppn=4,walltime=2:00:00

XCOREF_HOME=/iesl/canvas/absmasti/xcoref/

cd $XCOREF_HOME
mvn -X scala:run -Dlauncher=entity-linking
