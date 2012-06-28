# 1 -- patterns
# 2 -- sentences
mvn -q -e -f ../pom.xml compile exec:java -Dexec.mainClass=edu.washington.cs.knowitall.pattern.PatternExtractor -Dexec.args="--patterns $1 --sentences $2"
