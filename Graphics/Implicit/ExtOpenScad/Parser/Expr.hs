module Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0) where

import Graphics.Implicit.Definitions
import Text.ParserCombinators.Parsec  hiding (State)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Util

variable :: Parser Expr
variable = fmap Var variableSymb

literal :: Parser Expr
literal = ("literal" ?:) $ lexeme $
    "boolean" ?: do
        b  <-      (symbol "true"  >> return True )
              *<|> (symbol "false" >> return False)
        return $ LitE $ OBool b
    *<|> "number" ?: (
      (\a b -> LitE $ ONum (read (a ++ "." ++ b) :: ℝ)) <$> (many1 digit) <* (char '.') <*> (many digit)
      *<|>  do
            a <- many1 digit
            return $ LitE $ ONum (read a :: ℝ)
        )
    *<|> "string" ?: do
        _ <- string "\""
        strlit <-  many $ (string "\\\"" >> return '\"') 
                     *<|> (string "\\n" >> return '\n')
                     *<|> ( noneOf "\"\n")
        _ <- string "\""
        return $ LitE $ OString strlit

listSplice :: Parser (Expr -> Expr)
listSplice =
  brackets splice
  where
    splice = do
      start <- optionMaybe expr0
      _ <- charSep ':'
      end   <- optionMaybe expr0
      return $ case (start, end) of
                (Nothing, Nothing) -> id
                (Just s,  Nothing)  -> \l -> Var "splice" :$ [l, s, LitE OUndefined ]
                (Nothing, Just e )  -> \l -> Var "splice" :$ [l, LitE $ ONum 0, e]
                (Just s,  Just e )  -> \l -> Var "splice" :$ [l, s, e]

-- Operations with a one-to-one correspondence in operator name
unop :: Integer -> String -> Parser Expr
unop fixity op = (\expr -> Var op :$ [expr]) <$> (symbol op *> exprN fixity)

binop :: Integer -> Integer -> Parser String -> Parser Expr
binop f1 f2 op =
  (\a op b -> Var op :$ [a, b]) <$> exprN f1 <*> op <*> exprN f2

-- We represent the priority or 'fixity' of different types of expressions
-- by the Int argument

expr0 :: Parser Expr
expr0 = exprN 0

exprN :: Integer -> Parser Expr

exprN 12 =
         literal
    *<|> variable
    *<|> "parenthesized expression" ?: parens expr0
    *<|> "vector/list" ?: (
      -- eg. [ 1, 2, 3 ] or ( 1,2,3 )
      ListE <$> brackets (sepBy expr0 (charSep ','))
      *<|> ListE <$> parens (sepBy expr0 (charSep ',' ))
      )
    *<|> "vector/list generator" ?: do
        -- eg.  [ a : 1 : a + 10 ]
        exprs <- brackets (sepBy expr0 (charSep ':'))
        return $ collector "list_gen" exprs

exprN n@11 =
    do
        obj <- exprN $ n+1
        mods <- many1 (
            "function application" ?: do
               args <- parens (sepBy expr0 comma)
               return $ \f -> f :$ args
            *<|> "list indexing" ?: do
                i <- brackets expr0
                return $ \l -> Var "index" :$ [l, i]
            *<|> "list splicing" ?: listSplice
            )
        return $ foldl (\a b -> b a) obj mods
    *<|> (exprN $ n+1 )

exprN n@10 = 
    "negation" ?: unop (n + 1) "-"
    *<|> charSep '+' *> exprN (n + 1)
    *<|> exprN (n + 1)

exprN n@9 = 
    "exponentiation" ?: binop (n + 1) n (symbol "^")
    *<|> exprN (n+1)

exprN n@8 = 
    "multiplication/division" ?: do 
        -- outer list is multiplication, inner division.
        -- eg. "1*2*3/4/5*6*7/8"
        --     [[1],[2],[3,4,5],[6],[7,8]]
        exprs <- sepBy1 
            (sepBy1 (exprN $ n+1) (try $ charSep '/' )) 
            (try $ charSep '*' )
        let div  a b = Var "/" :$ [a, b]
        return $ collector "*" $ map (foldl1 div) exprs
    *<|> exprN (n+1)

exprN n@7 =
    "modulo" ?: do 
        exprs <- sepBy1 (exprN $ n+1) (try $ charSep '%')
        let mod  a b = Var "%" :$ [a, b]
        return $ foldl1 mod exprs 
    *<|> exprN (n+1)

exprN n@6 =
    "append" ?: (collector "++") <$> sepBy1 (exprN $ n+1) (try $ symbol "++")
    *<|> exprN (n+1)

exprN n@5 =
    "addition/subtraction" ?: do 
        -- Similar to multiply & divide
        -- eg. "1+2+3-4-5+6-7" 
        --     [[1],[2],[3,4,5],[6,7]]
        exprs <- sepBy1 
            (sepBy1 (exprN $ n+1) (try $ charSep '-' )) 
            (try $ charSep '+' )
        let maybeSub (x:xs)
              | null xs   = x
              | otherwise = Var "-" :$ (x:xs)
        return $ collector "+" $ map maybeSub exprs
    *<|> exprN (n+1)

exprN n@4 = 
    do
        firstExpr <- exprN $ n+1
        otherComparisonsExpr <- many $ do
            comparisonSymb <-
                     symbol "=="
                *<|> symbol "!="
                *<|> symbol ">="
                *<|> symbol "<="
                *<|> symbol ">"
                *<|> symbol "<"
            expr <- exprN $ n+1
            return (Var comparisonSymb, expr) 
        let
            (comparisons, otherExprs) = unzip otherComparisonsExpr
            exprs = firstExpr:otherExprs
        return $ case comparisons of 
            []  -> firstExpr
            [x] -> x :$ exprs
            _   -> collector "all" $ zipWith3 (\c e1 e2 -> c :$ [e1,e2]) comparisons exprs (tail exprs)
    *<|> exprN (n+1)

exprN n@3 =
    "logical-not" ?: unop (n + 1) "!"
    *<|> exprN (n+1)

exprN n@2 = 
    "logical and/or" ?: binop n' n' ((symbol "&&") <|> (symbol "||"))
    *<|> exprN n'
    where
      n' = n + 1

exprN n@1 = 
    "ternary" ?: do 
        a <- exprN $ n+1
        _ <- symbol "?"
        b <- exprN n
        _ <- symbol ":"
        c <- exprN n
        return $ Var "?" :$ [a,b,c]
    *<|> exprN (n+1)

exprN n@0 = lexeme (exprN $ n+1)

