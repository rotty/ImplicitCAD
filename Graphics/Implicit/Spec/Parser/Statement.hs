module Graphics.Implicit.Spec.Parser.Statement (statementSpec) where

import Test.Hspec
import Graphics.Implicit.Spec.Parser.Util
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Statement
import Data.Either

parsesAs :: String -> [StatementI] -> Expectation
parsesAs source stmts =
  (parseProgram "src" source) `shouldBe` Right stmts

parsesAsError :: String -> Expectation
parsesAsError source =
  (parseProgram "src" source) `shouldSatisfy` isLeft

single :: Statement StatementI -> [StatementI]
single st = [StatementI 1 st]

call :: Symbol -> [(Maybe Symbol, Expr)] -> [StatementI] -> StatementI
call name args stmts = StatementI 1 (ModuleCall name args stmts)

ifSpec :: Spec
ifSpec = do
  it "parses" $
    "if (true) { a(); } else { b(); }" `parsesAs` (
      single $ If (bool True) [call "a" [] []] [call "b" [] []])

assignmentSpec :: Spec
assignmentSpec = do
  it "parses correctly" $
    "y = -5;" `parsesAs` (single $ Name "y" := (Var "-" :$ [num 5.0]))
  it "handles pattern matching" $
    "[x, y] = [1, 2];" `parsesAs`
    (single $ ListP [Name "x", Name "y"] := (ListE [num 1, num 2]))
  it "handles function definitions" $
    "foo (x, y) = x * y;" `parsesAs` single fooFunction
  it "handles the function keyword" $
    "function foo(x, y) = x * y;" `parsesAs` single fooFunction
  where
    fooFunction = Name "foo" := (LamE [Name "x", Name "y"]
                                 (Var "*" :$ [ListE [Var "x", Var "y"]]))

statementSpec :: Spec
statementSpec = do
  describe "empty file" $ do
    it "returns an empty list" $
      "" `parsesAs` []

  describe "difference of two cylinders" $ do
    it "parses correctly" $
      "difference(){ cylinder(r=5,h=20); cylinder(r=2,h=20); }"
      `parsesAs` single (
        ModuleCall "difference" [] [
           (call "cylinder" [(Just "r", num 5.0),
                             (Just "h", num 20.0)]
            []),
           (call "cylinder" [(Just "r", num 2.0),
                             (Just "h", num 20.0)]
            [])])

  describe "empty module definition" $ do
    it "parses correctly" $
      "module foo_bar() {}" `parsesAs` (single $ NewModule "foo_bar" [] [])

  describe "assignment" assignmentSpec

  describe "if" ifSpec
