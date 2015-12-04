module Graphics.Implicit.Spec.Parser.Util (num, bool) where

import Graphics.Implicit.Definitions
import Graphics.Implicit.ExtOpenScad.Definitions

num :: ℝ -> Expr
num x = LitE (ONum x)

bool :: Bool -> Expr
bool = LitE . OBool
