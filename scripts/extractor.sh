echo "$*"
mvn -q -e exec:java -Dexec.mainClass=edu.washington.cs.knowitall.pattern.OpenParse -Dexec.args="$*"
