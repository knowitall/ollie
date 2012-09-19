# Ollie

Ollie is a program that automatically identifies and extracts binary
relationships from English sentences.  Ollie is designed for Web-scale
information extraction, where target relations are not specified in advance.

Ollie is our second-generation information extraction system .  Whereas <a
href="http://reverb.cs.washington.edu/">ReVerb</a> operates on flat sequences
of tokens, Ollie works with the tree-like (graph with only small cycles)
representation using Stanford's compression of the dependencies.  This allows
Ollie to capture expression that ReVerb misses, such as long-range relations.

Ollie also captures context that modifies a binary relation.  Presently Ollie
handles attribution (He said/she believes) and enabling conditions (if X
then).

## Examples

### Enabling Condition

An enabling condition is a condition that needs to be met for the extraction to
be true.  Certain words demark an enabling condition, such as "if" and "when".
Ollie captures enabling conditions if they are present.

    sentence: If I slept past noon, I'd be late for work.
    extraction: (I; 'd be late for; work)[enabler=If I slept past noon]

### Attribution

An attribution clause specifies an entity that asserted an extraction and a
verb that specifies the expression.  Ollie captures attributions if they are
present.

    sentence: Some people say Barack Obama was not born in the United States.
    extraction: (Barack Obama; was not born in; the United States)[attrib=Some people say]

    sentence: Early astronomers believe that the earth is the center of the universe.
    extraction: (the earth; is the center of; the universe)[attrib=Early astronomers believe]

### Long-range extractions

Unlike ReVerb, Ollie can capture long-range relations that are expressed far
apart in the sentence but near in the dependency graph.

    sentence: After winning the Superbowl, the Saints are now the top dogs of the NFL.
    extraction: (the Saints; are now the top dogs of; the NFL)

### Relational noun

Some relations are expressed without verbs.  Ollie can capture these as well as
verb-mediated relations.

    sentence: Microsoft co-founder Bill Gates spoke at a conference on Monday.
    extraction: (Bill Gates; be co-founder of; Microsoft)


### N-ary extractions

Often times similar relations will specify different aspects of the same event.
Since Ollie captures long-range relations it can capture N-ary extractions by
collapsing extractions where the relation phrase only differs by the
preposition.

    sentence: I learned that the 2012 Sasquatch music festival is scheduled for May 25th until May 28th.
    extraction: (the 2012 Sasquatch music festival; is scheduled for; May 25th)
    extraction: (the 2012 Sasquatch music festival; is scheduled until; May 28th)
    nary: (the 2012 Sasquatch music festival; is scheduled; [for May 25th; to May 28th])

## Quick Start

If you want to run Ollie on a small amount of text without modifying the source
code, you can use an executable file that can be run from the command line.
Follow these steps to get started:

1.  Download the latest Ollie binary from:
    http://knowitall.cs.washington.edu/ollie/ollie-app-latest.jar

2.  Download the linear English MaltParser model (engmalt.linear-1.7.mco) from:
    http://www.maltparser.org/mco/english_parser/engmalt.html
    and place it in the same directory as Ollie.

3.  Run `java -Xmx512m -jar ollie-app-latest.jar yourfile.txt`.  Omit the
    input file for an interactive console.

## Building

Building Ollie from source requires Apache Maven (<http://maven.apache.org>).
First, clone or download the Ollie source from GitHub.  Run this command in the
top-level source folder to download the required dependencies, compile, and
create a single jar file.

    mvn clean package

The compiled class files will be put in the base directory.  The single
executable jar file will be written to `ollie-app-VERSION.jar` where `VERSION`
is the version number.

## Command Line Interface

Once you have built Ollie, you can run it from the command line.

    java -Xmx512m -jar ollie-app-VERSION.jar yourfile.txt

Omit the input file for an interactive console.

Ollie takes sentences, one-per-line as input or splits text into sentences if
`--split` is specified.  Run Ollie with `--usage` to see full usage.  There are
many useful options; for example you can run Ollie in parallel using
`--parallel`.

## Graphical Interface

Ollie works ontop of a subcomponent called OpenParse.  The distinction is
largely technical; OpenParse does not handle attribution and enabling condition
and uses a coarser confidence metric.  You can use a GUI application to
visualize the OpenParse extractions in a parse tree.  To use it, you will need
to have [graphviz](http://www.graphviz.org/) installed.  You can run the GUI
with:

    java -Xms512M -Xmx1g -cp ollie-app-VERSION.jar edu.washington.cs.knowitall.openparse.OpenParseGui

By default, this application will look for graphviz's `dot` program at
`/usr/bin/dot`.  You can specify a location with the `--graphviz` parameter.

You can try out your own models with `Options->Load Model...`.  To see an
example model, look at `openparse.model` in `src/main/resources`. Your model
may have one or more patterns in it.  If you want to see pattern matches
(without node expansion) instead of triple extractions, you can choose to show
the raw match with `Options->Raw Matches`.  This will allow you to use patterns
that do not capture an arg1, rel, and arg2.  

## Parsers

Ollie is packaged to use Malt Parser, one of the fastest dependency parsers
available.  You will need the model file (`engmalt.linear-1.7.mco`) in the
directory the application is run from or you will need to specify its location
with the `--malt-model` parameter.  Malt Parser models are available online.

  http://www.maltparser.org/mco/english_parser/engmalt.html

Ollie works with any other parser in the `nlptools` project.  For example, it
is easy to swap out Malt for Stanford's parser.  Stanford's parser is not an
option by default because of licensing conflicts, but it was used for some
of the results in the paper.

## Using Eclipse

To modify the Ollie source code in Eclipse, use the [M2Eclipse
plugin](http://www.sonatype.org/m2eclipse/) along with
[ScalaIDE](http://scala-ide.org/).  You can then import the project using 
the following.

    File > Import > Existing Maven Projects

## Including Ollie as a Dependency

Add the following as a Maven dependency.

    <groupId>edu.washington.cs.knowitall.ollie</groupId>
    <artifactId>ollie-core_2.9.2</artifactId>
    <version>[1.0.0, )</version>

The best way to find the latest version is to browse [Maven Central](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22edu.washington.cs.knowitall%22).

`ollie-core` does not include a way to parse sentences.  You will need to use a
parser supplied by the [nlptools](https://github.com/knowitall/nlptools)
project.  The source for for `ollie-app` is an excellent example of a project
using `ollie-core` as a dependency.  `ollie-app` supplies a parser from
[nlptools](https://github.com/knowitall/nlptools).

## Concurrency

When operating at web scale, parallelism is essential.  While the base Ollie
extractor is immutable and thread safe, the parser may not be thread safe.  I
do not know whether Malt parser is thread safe.

## Contact

To contact the UW about Ollie, email knowit-ollie@cs.washington.edu.

## Contributors
* Michael Schmitz (http://www.schmitztech.com)
* Robert Bart (rbart at cs.washington.edu)

## Citing Ollie
If you use Ollie in your academic work, please cite Ollie with the following 
BibTeX citation:

    @inproceedings{ollie-emnlp12,
      author = {Mausam and Michael Schmitz and Robert Bart and Stephen Soderland and Oren Etzioni},
      title = {Open Language Learning for Information Extraction},
      booktitle = {Proceedings of Conference on Empirical Methods in Natural Language Processing and Computational Natural Language Learning (EMNLP-CONLL)},
      year = {2012}
    }
