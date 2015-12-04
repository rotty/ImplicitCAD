module Graphics.Implicit.Spec (spec) where

import Test.Hspec
import Graphics.Implicit.Spec.Parser

spec :: Spec
spec = do
  describe "parser" parserSpec

