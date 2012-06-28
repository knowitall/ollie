# 1 -- lda directory
cut -f5 "$1/raw/patterned-all.txt" | sort | uniq -c | sort -nr > "$1/raw/patterns.txt"
mvn -q -e exec:java -Dexec.mainClass=edu.washington.cs.knowitall.pattern.KeepCommonPatterns -Dexec.args="$1/raw/patterned-all.txt 10" > "$1/raw/patterned.txt"
