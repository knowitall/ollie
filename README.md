# Ollie

Ollie is a program that automatically identifies and extracts binary
relationships from English sentences.  Ollie is designed for Web-scale
information extraction, where the target relations are not specified in
advance and performance is important.

Ollie is our second-generation information extraction system superceding
ReVerb.  Whereas ReVerb operates on flat sequences of tokens, Ollie operates on
the tree-like (graph with only small cycles) representation of a sentence using
Stanford's compression of the dependencies.  This allows Ollie to capture
expression that ReVerb misses, such as long-range relations.

Ollie also captures context in addition to a binary relation.  Presently Ollie
handles attribution (He said/she believes) and enabling conditions (if X
then...).

## Quick Start

If you want to run Ollie on a small amount of text without modifying the source
code, you can use an executable file that can be run from the command line.
Follow these steps to get started:

1.  Download the latest Ollie source from <TODO>

2.  Run `mvn clean package assembly:single`.

3.  Run `java -Xmx512m -jar target/ollie.jar yourfile.txt`.  Omit the file for
    an interactive console.

## Building

Building Ollie from source requires Apache Maven (<http://maven.apache.org>).
Run this command to download the required dependencies, compile, and create a
single jar file.

    mvn clean package assembly:single

The compiled class files will be put in the `target/classes` directory.  The
single executable jar file will be written to
`target/ollie-*-jar-with-dependencies.jar` where `*` is the version number.

## Command Line Interface

Once you have build Ollie, you can run it from the command line.  Ollie takes
sentences, one-per-line as input.

## Using Eclipse

To modify the Ollie source code in Eclipse, use the M2Eclipse plugin along with
ScalaIDE.  You can then import the project using File > Import > 
Existing Maven Projects.

## Including Ollie as a Dependency

## Contributors
* Michael Schmitz <http://www.schmitztech.com>>
* Robert Bart (rbart at cs.washington.edu)

## Citing Ollie
If you use Ollie in your academic work, please cite Ollie with the following 
BibTeX citation:

TODO
