mkdir "$1/templates/"
mvn exec:java -Dexec.mainClass=edu.washington.cs.knowitall.pattern.BuildTemplates -Dexec.args="$1/raw/patterned.txt $1/templates/templates.txt --reltemplates $HOME/public/read/reltemplates.txt --debug $1/templates/"
