# $1 -- lda base folder
STATS=$1/stats
mkdir -p $STATS
cd ..
mvn -q -e compile exec:java -Dexec.mainClass=edu.washington.cs.knowitall.pattern.lda.Distributions -Dexec.args="'$1' '$STATS'"
