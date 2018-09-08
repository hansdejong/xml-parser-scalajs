This is a simple XML-parser for Scala / Scala.js.

I made it because I wanted to move some Java applets using XML to Scala.js.<br>
Native Scala XML-support didn't seem flexible enough.<br>
(Scala.js compiles Scala-code to Javascript but can't handle Java-code and -libraries. Scala itself often uses Java-libraries.)<br>
At first I tried to use [FastParse](http://www.lihaoyi.com/fastparse/).
I made an XML-parser with it, and one for evaluating Double-expressions,
but couldn't fix some problems.

This time I ported a regex-[Python-Lexer](http://code.activestate.com/recipes/65125-xml-lexing-shallow-parsing/)
to Scala and extended it to produce a hierarchical Scala-datastructure.

 I made it in [Ammonite](http://ammonite.io/#Ammonite).
 To check it out, just install Ammonite, and run **amm -w P1_opstart.sc** on it.
 It is easy to build into a Scala-application.
 It is tested on the provided listing in Scala.js (that was a tense moment, using regex in Javascript).<br>
 (I'm not sure how easy it is to run Ammonite under Windows.)

 Until now I never used it in a real application, so the interfacing is very immature,
  but the example-code shows you a direction.<br>
I intend to also show a Scala.js app using it on my blog. Working on it.
