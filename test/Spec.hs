import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Ord
import Lexer as L
import Parser.Parser as P
import Parser.Immediate as PI ( jimpleImmediate )
import Parser.Modifier as PM ( jimpleModifier )
import qualified Parser.Expression as PE
import Test.Tasty
import Test.Tasty.HUnit ( (@?=), testCase, assertFailure )
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import qualified Utils as U
import qualified Parser.Utils as PU

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, acceptanceTests]

parsedCorrectly str mode = case res of
  Left err -> assertFailure $ "Unable to parse string: " ++ str
  Right ok -> return ()
  where
    res = P.unitParseTest mode str

parsedIncorrectly str mode = case res of
  Left err -> return ()
  Right ok -> assertFailure $ "Shouldn't parse input: " ++ str
  where
    res = P.unitParseTest mode str

correctTest name f testString = testCase name $ parsedCorrectly testString f

incorrectTest name f testString = testCase (name ++ " (false)") $ parsedIncorrectly testString f

unitTests :: TestTree
unitTests =
  testGroup
    "Parsing tests"
    [ correctTest "Abstract test" PM.jimpleModifier "abstract",
      incorrectTest "Abstract test" PM.jimpleModifier "abstracta",
      correctTest "Basic identifier test" L.identifier "abstract",
      correctTest "Basic identifier test 2" L.identifier "i6",
      incorrectTest "Basic identifier test" L.identifier "91234",
      correctTest "Basic identifier test" L.identifier "\\tes\\t",
      correctTest "Basic identifier test 2 " L.identifier "$ri3",
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
      correctTest "Declaration test" P.jimpleDeclaration "bool test;",
      correctTest "Declaration test 2" P.jimpleDeclaration "bool[][] test;",
      correctTest "Declaration test 2" P.jimpleDeclaration "bool[][] test,test1;",
      correctTest "Declaration test 3" P.jimpleDeclaration "boolean $z0;",
      correctTest "Full Function test 2" P.jimpleClassMember "void <init>() {  HelloWorld r0; r0 := @this: HelloWorld; specialinvoke r0.<java.lang.Object: void <init>()>(); return;}",
      correctTest "Class with modifier test" P.jimpleFile "public class HelloWorld extends java.lang.Object { }",
      correctTest "Class method identity test" P.jimpleStatementReturn "return $i0;",
      correctTest "Class method throws test" P.jimpleClassMemberMethod "public static int main(java.lang.String[]) throws java.lang.Exception { } ",
      correctTest "At identifier this" L.atIdentifier "@this:",
      correctTest "At identifier caughtexception" L.atIdentifier "@caughtexception",
      correctTest "At identifier parameter" L.atIdentifier "@parameter3:",
      incorrectTest "At identifier parameter" L.atIdentifier "@parameter:",
      incorrectTest "At identifier parameter" L.atIdentifier "@parameter3",
      correctTest "Invoke Statement" P.jimpleStatementInvoke "specialinvoke $r0.<java.util.Random: void <init>()>();",
      correctTest "Assignment Statement" P.jimpleStatementAssignment "$r0 = new java.util.Random;",
      correctTest "Assignment Statement" P.jimpleStatementAssignment "$$i0 = virtualinvoke $r0.<java.util.Random: int nextInt(int)>(30);",
      correctTest "Bool Expression" PE.jimpleExpression "a == 10",    
      correctTest "Bool Expression 2" PE.jimpleExpression "a - 10",     
      correctTest "Bool Expression 3" PE.jimpleExpression "20 == i6",  
      correctTest "Bool Expression 4" PE.jimpleExpression "20 == 10",
      correctTest "Bool Expression 5" PE.jimpleExpression "a - a",
      correctTest "Bool Expression 6" PE.jimpleExpression "10 + 50",
      correctTest "Bool Expression 7" PE.jimpleExpression "i5 >= 50",
      correctTest "Newarray Expression" PE.jimpleNewArrayExpression "newarray (int)[20]",
      correctTest "Dereference Expression" PE.jimpleReferenceExpr "i2[10]",
      correctTest "Dereference Expression 2" PE.jimpleReferenceExpr "r0[i4]",
      correctTest "Dereference Expression 3" PE.jimpleReferenceExpr "r0[$i3]",
      correctTest "Dereference Assignment" P.jimpleStatementAssignment "i2[10] = 0;",
      correctTest "If Statement" P.jimpleStatementIfGoto "if a == 10 goto label1;",
      correctTest "If Statement 2" P.jimpleStatementIfGoto "if a != 10 goto label1;",
      correctTest "FieldAccess Statement" PU.jimpleFieldReference "<kotlin._Assertions: boolean ENABLED>",
      correctTest "FieldAccess Statement" PU.jimpleFieldReference "r0.<Foo: int A>",
      correctTest "Special Invoke Statement" P.jimpleStatementInvoke "specialinvoke $r0.<java.lang.AssertionError: void <init>(java.lang.Object)>(\"Assertion failed\");",
      correctTest "Throw Statement" P.jimpleStatementThrow "throw $r0;",
      correctTest "Immediate Number" PI.jimpleImmediate "0",
      correctTest "Immediate Number 2" PI.jimpleImmediate "20",
      correctTest "Immediate Variable" PI.jimpleImmediate "i6",
      correctTest "Immediate Variable 2" PI.jimpleImmediate "$i3",
      correctTest "Immediate String" PI.jimpleImmediate "\"qwe asd qweew\"",
      correctTest "Assignment to Immediate" P.jimpleStatementAssignment "$z1 = 0;",
      correctTest "Assignment to Deref 2" P.jimpleStatementAssignment "$i4 = r0[$i3];",
      correctTest "Assignment to Expression" P.jimpleStatementAssignment "z1 = 20 - i6;",
      correctTest "Assignment to Expression 2" P.jimpleStatement " $i2 = 20 - i4;",
      correctTest "Assignment to Deref" P.jimpleStatementAssignment " $i2 = r0[i4];"
      -- TODO: incorrectTest "Declaration test 3" P.jimpleDeclaration  "return;"
    ]

compareFiles :: FilePath -> FilePath -> IO ()
compareFiles a b = do
  aContents <- readFile a
  bContents <- B.readFile b
  let processed = (U.process aContents)
  case processed of
    Nothing -> assertFailure "Couldn't parse Jimple File"
    Just x -> x @?= (bContents)

acceptanceTestCase name fileA fileB = testCase name $ compareFiles fileA fileB

acceptanceTests :: TestTree
acceptanceTests =
  testGroup
    "Acceptance Test"
    [
      acceptanceTestCase "Hello True" "test/hello-true.jimple" "test/hello-true.expected",
      acceptanceTestCase "Hello False" "test/hello-false.jimple" "test/hello-false.expected",
      acceptanceTestCase "Array True" "test/array-true.jimple" "test/array-true.expected",
      acceptanceTestCase "Array False" "test/array-false.jimple" "test/array-false.expected",
      acceptanceTestCase "Func Call False" "test/func-call-false.jimple" "test/func-call-false.expected",
      acceptanceTestCase "Func Call 2 False" "test/func-call-2-false.jimple" "test/func-call-2-false.expected",
      acceptanceTestCase "Sort True" "test/sort-true.jimple" "test/sort-true.expected"
    ]
