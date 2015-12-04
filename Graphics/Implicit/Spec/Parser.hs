module Graphics.Implicit.Spec.Parser (parserSpec) where

import Test.Hspec
import Graphics.Implicit.Spec.Parser.Expr
import Graphics.Implicit.Spec.Parser.Statement

parserSpec :: Spec
parserSpec = do
  describe "expressions" exprSpec
  describe "statements" statementSpec
