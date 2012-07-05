# Ollie

Ollie is a program that automatically identifies and extracts binary
relationships from English sentences.  Ollie is designed for Web-scale
information extraction, where target relations are not specified in advance.

Ollie is our second-generation information extraction system superceding
ReVerb.  Whereas ReVerb operates on flat sequences of tokens, Ollie operates on
the tree-like (graph with only small cycles) representation of a sentence using
Stanford's compression of the dependencies.  This allows Ollie to capture
expression that ReVerb misses, such as long-range relations.

Ollie also captures context that modifies a binary relation.  Presently Ollie
handles attribution (He said/she believes) and enabling conditions (if X
then...).

## Quick Start

If you want to run Ollie on a small amount of text without modifying the source
code, you can use an executable file that can be run from the command line.
Follow these steps to get started:

1.  Download the latest Ollie source from https://github.com/knowitall/ollie.

2.  Run `mvn clean package`.

3.  Run `java -Xmx512m -jar ollie-app-VERSION.jar yourfile.txt`.  Omit the
    input file for an interactive console.

## Building

Building Ollie from source requires Apache Maven (<http://maven.apache.org>).
Run this command to download the required dependencies, compile, and create a
single jar file.

    mvn clean package

The compiled class files will be put in the base directory.  The single
executable jar file will be written to `ollie-app-VERSION.jar` where `VERSION`
is the version number.

## Command Line Interface

Once you have build Ollie, you can run it from the command line.  Ollie takes
sentences, one-per-line as input or splits text into sentences if `--split` is
specified.  Run Ollie with `--usage` to see full usage.  There are many
useful options; for example you can run Ollie in parallel using `--parallel`.

## Using Eclipse

To modify the Ollie source code in Eclipse, use the [M2Eclipse
plugin](http://www.sonatype.org/m2eclipse/) along with ScalaIDE.  You can then
import the project using File > Import > Existing Maven Projects.

## Including Ollie as a Dependency

Add the following as a Maven dependency.

    <groupId>edu.washington.cs.knowitall.ollie</groupId>
    <artifactId>ollie-core_2.9.2</artifactId>
    <version>[1.0.0, )</version>

The best way to find the latest version is to browse [Maven Central](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22edu.washington.cs.knowitall%22).

`ollie-core` does not include a way to parse sentences.  You will need to use a
parser supplied by the [nlptools](https://github.com/knowitall/nlptools)
project.  The source for for `ollie-app` is an excellent example.

## Contributors
* Michael Schmitz <http://www.schmitztech.com>>
* Robert Bart (rbart at cs.washington.edu)

## Citing Ollie
If you use Ollie in your academic work, please cite Ollie with the following 
BibTeX citation:

TODO
