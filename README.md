# jimple to JSON

This is a Haskell application that parses multiple jimple files and converts
them into JSON

Ths is still a WIP in the Early Stages

## Building

Use [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) tool for
setup, or use cabal directly

`stack setup`

## Running

Again, if you are using Stack it is just

`stack exec jimple-exe <file.jimple>`

## Testing

To run the tests:

`stack exec jimple-exe <file.jimple>`

## Debugging

You can create a jimple from a Java file by:

1. Download [Soot](https://repo1.maven.org/maven2/org/soot-oss/soot/4.2.1/soot-4.2.1-jar-with-dependencies.jar)
2. Generate the .class file using `javac HelloWorld.java`
3. Run soot: `java -cp soot-4.2.1-jar-with-dependencies.jar soot.Main -cp . -pp -f jimple HelloWorld`

For Kotlin files, the process is somewhat different.

1. Download [Soot](https://repo1.maven.org/maven2/org/soot-oss/soot/4.2.1/soot-4.2.1-jar-with-dependencies.jar)
2. Get the Kotlin Runtime build `kotlinc -include-runtime -d output.jar ../<file>.kt`
3. Extract the Jar `jar xf output.jar`
4. Run soot ` java -cp $HOME/soot-4.3.0-jar-with-dependencies.jar soot.Main -cp . -pp -f jimple OriginalKt -write-local-annotations -p jb use-original-names:true -keep-line-number -print-tags-in-output` 
