# 1 -- lda directory
ROWS="$1/raw/patterned.txt"
TEST="$1/raw/test.txt"
TRAIN="$1/raw/train.txt"
mvn -q -e compile exec:java -Dexec.mainClass=edu.washington.cs.knowitall.pattern.lda.CreateTestSet -Dexec.args="$ROWS $TEST $TRAIN"

