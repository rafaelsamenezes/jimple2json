import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Parser as P
import Lexer as L

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

-- TODO: Add property tests
properties :: TestTree
properties = testGroup "Properties" []
--properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

parsedCorrectly str mode = case res of
    Left err -> assertFailure $ "Unable to parse string: " ++ str
    Right ok -> return()
    where
        res = P.unitParseTest mode str

parsedIncorrectly str mode = case res of
    Left err -> return()
    Right ok -> assertFailure $ "Shouldn't parse input: " ++ str
    where
        res = P.unitParseTest mode str


correctTest name f testString = testCase name $ parsedCorrectly testString f
incorrectTest name f testString = testCase (name ++ " (false)") $ parsedIncorrectly testString f

unitTests = testGroup "Parsing tests"
  [
    correctTest "Abstract test" P.jimpleModifierAbstract "abstract",
    incorrectTest "Abstract test" P.jimpleModifierAbstract "abstracta",
    correctTest "Basic identifier test" L.identifier "abstract",
    incorrectTest "Basic identifier test" L.identifier "91234",
    correctTest "Basic identifier test" L.identifier "\\tes\\t",
    correctTest "Basic identifier test <init>" L.identifier "<init>",
    correctTest "Basic identifier test <clinit>" L.identifier "<clinit>",
    incorrectTest "Basic identifier test <asd> (Fail)" L.identifier "<asd>",
    correctTest "Full identifier test" L.fullIdentifier "asd.qwe",
    correctTest "Initial class test" P.jimpleFile "class HelloWorld { }",
    correctTest "Extended class test" P.jimpleFile "class HelloWorld extends java.lang.Object { }",
    correctTest "Class field member test" P.jimpleClassMember "static final boolean $assertionsDisabled;",
    correctTest "Class with field member test" P.jimpleFile "class HelloWorld extends java.lang.Object { static final boolean $assertionsDisabled; }",
    correctTest "Class empty method test" P.jimpleClassMemberMethod "void <init>();",
    correctTest "Class method test" P.jimpleClassMemberMethod "void <init>() {HelloWorld r0;}",
    correctTest "Full Class method test" P.jimpleFile "class HelloWorld {void <init>() {HelloWorld r0;} }",
    correctTest "Class method identity test" P.jimpleClassMemberMethod "void <init>() { r0 := @this: HelloWorld;}",
    correctTest "Class method identity test" P.jimpleClassMemberMethod "void <init>() { specialinvoke r0.<java.lang.Object: void <init>()>();}",
    correctTest "Class method identity test" P.jimpleClassMemberMethod "void <init>() { return;}",
    correctTest "Declaration test" P.jimpleDeclaration  "bool test;",
    correctTest "Declaration test 2" P.jimpleDeclaration  "bool[][] test;",
    correctTest "Declaration test 2" P.jimpleDeclaration  "bool[][] test,test1;",
    correctTest "Full Function test 2" P.jimpleClassMember  "void <init>() {  HelloWorld r0; r0 := @this: HelloWorld; specialinvoke r0.<java.lang.Object: void <init>()>(); return;}",
    correctTest "Class with modifier test" P.jimpleFile "public class HelloWorld extends java.lang.Object { }",
    correctTest "Class method identity test" P.jimpleStatementReturn "return $i0;",
    correctTest "Class method throws test" P.jimpleClassMemberMethod "public static int main(java.lang.String[]) throws java.lang.Exception { } ",
    correctTest "At identifier this" L.atIdentifier "@this:",
    correctTest "At identifier caughtexception" L.atIdentifier "@caughtexception",
    correctTest "At identifier parameter" L.atIdentifier "@parameter3:",
    incorrectTest "At identifier parameter" L.atIdentifier "@parameter:",
    incorrectTest "At identifier parameter" L.atIdentifier "@parameter3",
    correctTest "Invoke Statement" P.jimpleStatementSpecialInvoke  "specialinvoke $r0.<java.util.Random: void <init>()>()",
    correctTest "Assignment Statement" P.jimpleStatementAssignment   "$r0 = new java.util.Random;",
    correctTest "Assignment Statement" P.jimpleStatementAssignment   "$$i0 = virtualinvoke $r0.<java.util.Random: int nextInt(int)>(30);",
    correctTest "Bool Expression" P.jimpleBoolExpr   "a == 10",
    correctTest "If Statement" P.jimpleStatementIfGoto "if a == 10 goto label1;"
    -- TODO: incorrectTest "Declaration test 3" P.jimpleDeclaration  "return;"
  ]