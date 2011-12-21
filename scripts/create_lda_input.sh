# 1 -- lda directory
mkdir -p "$1/ldainput"
mvn -q -e exec:java -Dexec.mainClass=edu.washington.cs.knowitall.pattern.lda.CreateLDAFiles -Dexec.args="'$1/raw/train.txt' '$1/ldainput'"
