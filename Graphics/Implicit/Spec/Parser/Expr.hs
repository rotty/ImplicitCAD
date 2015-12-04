module Graphics.Implicit.Spec.Parser.Expr (exprSpec) where

import Test.Hspec
import Graphics.Implicit.Spec.Parser.Util
import Text.ParserCombinators.Parsec hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Util
import Graphics.Implicit.ExtOpenScad.Parser.Expr
import Data.Either

-- TODO:
-- * provide syntactic sugar for checking parseProgram

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse expr "expr" s where
  expr = do
    e <- expr0
    eof
    return e

app :: Symbol -> [Expr] -> Expr
app name args = Var name :$ [ListE args]

app' :: Symbol -> [Expr] -> Expr
app' name args = Var name :$ args

infixr 1 -->
(-->) :: String -> Expr -> Expectation
(-->) source expr =
  (parseExpr source) `shouldBe` Right expr

infixr 1 -->+
(-->+) :: String -> (Expr, String) -> Expectation
(-->+) source (expr, leftover) =
  (parseWithLeftOver expr0 source) `shouldBe` (Right (expr, leftover))

leftoverSpec :: Spec
leftoverSpec = do
  it "consumes entire comparison" $
    "1 > 0" -->+ (app' ">" [num 1, num 0], "")
  it "consumes entire comparison, but leaves stray question mark" $
    "1 > 0 ?" -->+ (app' ">" [num 1, num 0], "?")

logicalSpec :: Spec
logicalSpec = do
  it "handles not" $ "!foo" --> (app' "!" [Var "foo"])
  it "handles and/or" $ do
    "foo && bar" --> app' "&&" [Var "foo", Var "bar"]
    "foo || bar" --> app' "||" [Var "foo", Var "bar"]
  it "handles the ternary operator" $ do
    "x ? 2 : 3" --> app' "?" [Var "x", num 2, num 3]
    "1 > 0 ? 5 : -5" --> app' "?" [app' ">" [num 1, num 0], num 5, app' "-" [num 5]]
    "1 > 0 ? 5 : 1 + 2" -->
      app' "?" [app' ">" [num 1, num 0], num 5, app "+" [num 1, num 2]]

exprSpec :: Spec
exprSpec = do
  describe "leftover behavior" $ leftoverSpec
  describe "identifiers" $ do
    it "accepts valid variable names" $ do
      "foo" --> Var "foo"
      "foo_bar" --> Var "foo_bar"
  describe "literals" $ do
    it "handles integers" $ do
      parseExpr "12356" `shouldBe` (Right $ num 12356)
    it "handles floats" $ do
      parseExpr "23.42" `shouldBe` (Right $ num 23.42)
    it "handles booleans" $ do
      parseExpr "true" `shouldBe` (Right $ bool True)
      parseExpr "false" `shouldBe` (Right $ bool False)
  describe "grouping" $ do
    it "allows parens" $ do
      parseExpr "( false )" `shouldBe` (Right $ bool False)
    it "handles vectors" $ do
      parseExpr "[ 1, 2, 3 ]" `shouldBe` (Right $ ListE [num 1, num 2, num 3])
    it "handles lists" $ do
      parseExpr "( 1, 2, 3 )" `shouldBe` (Right $ ListE [num 1, num 2, num 3])
    it "handles generators" $
      parseExpr "[ a : 1 : b + 10 ]" `shouldBe`
      Right (app "list_gen" [Var "a", num 1, app "+" [Var "b", num 10]])
  describe "arithmetic" $ do
    it "handles unary +/-" $ do
      parseExpr "-42" `shouldBe` (Right $ app' "-" [num 42])
      parseExpr "+42" `shouldBe` (Right $ num 42)
    it "handles +" $ do
      "1 + 2" --> app "+" [num 1, num 2]
      "1 + 2 + 3" --> app "+" [num 1, num 2, num 3]
    it "handles -" $ do
      "1 - 2" --> app' "-" [num 1, num 2]
      "1 - 2 - 3" --> app' "-" [num 1, num 2, num 3]
    it "handles +/- in combination" $
      "1 + 2 - 3 + 4 - 5 - 6" --> app "+" [num 1,
                                           app' "-" [num 2, num 3],
                                           app' "-" [num 4, num 5, num 6]]
    it "handles exponentiation" $
      parseExpr "x ^ y" `shouldBe` (Right $ app' "^" [Var "x", Var "y"])
    it "handles *" $ do
      parseExpr "3 * 4" `shouldBe` (Right $ app "*" [num 3, num 4])
      parseExpr "3 * 4 * 5" `shouldBe` (Right $ app "*" [num 3, num 4, num 5])
    it "handles /" $
      parseExpr "4.2 / 2.3" `shouldBe` (Right $ app' "/" [num 4.2, num 2.3])
    it "handles precedence" $
      parseExpr "1 + 2 / 3 * 5" `shouldBe`
      (Right $ app "+" [num 1, app "*" [app' "/" [num 2, num 3], num 5]])
  it "handles append" $
    parseExpr "foo ++ bar ++ baz" `shouldBe`
    (Right $ app "++" [Var "foo", Var "bar", Var "baz"])
  describe "logical operators" logicalSpec
