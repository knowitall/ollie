# 1 -- lda directory
mvn -q -e exec:java -Dexec.mainClass=edu.washington.cs.knowitall.pattern.lda.ConvertLDAOutput -Dexec.args="$1/ldaoutput/output-raw.txt" > "$1/ldaoutput/output.txt"
