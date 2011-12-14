mvn -q -e compile exec:java -Dexec.mainClass=edu.washington.cs.knowitall.pattern.BuildTreePatterns -Dexec.args="$1/raw/parsed.txt $1/raw/patterned-all.txt --length 3" 2> $1/raw/patterned-all.log
