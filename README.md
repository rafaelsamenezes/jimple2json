# jimple_parser
A C++ jimple parser

This is still a WIP.

# Testing

You can create a jimple from a Java file by:

1. Download [Soot](https://repo1.maven.org/maven2/org/soot-oss/soot/4.2.1/soot-4.2.1-jar-with-dependencies.jar)
2. Generate the .class file using `javac HelloWorld.java`
3. Run soot: `java -cp soot-4.2.1-jar-with-dependencies.jar soot.Main -cp . -pp -f jimple HelloWorld`
