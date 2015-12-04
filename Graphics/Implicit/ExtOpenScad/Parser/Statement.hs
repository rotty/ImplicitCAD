module Graphics.Implicit.ExtOpenScad.Parser.Statement where

import Text.ParserCombinators.Parsec hiding (State)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad (void)
import Graphics.Implicit.ExtOpenScad.Definitions
import Graphics.Implicit.ExtOpenScad.Parser.Util
import Graphics.Implicit.ExtOpenScad.Parser.Expr

parseProgram :: SourceName -> [Char] -> Either ParseError [StatementI]
parseProgram name s = parse program name s where
    program = do
        sts <- many computation
        eof
        return sts

-- | A  in our programming openscad-like programming language.
computation :: Parser StatementI
computation = 
    do -- suite statements: no semicolon...
        whitespace *> tryMany [
            ifStatementI,
            forStatementI,
            throwAway,
            userModuleDeclaration{-,
            unimplemented "mirror",
            unimplemented "multmatrix",
            unimplemented "color",
            unimplemented "render",
            unimplemented "surface",
            unimplemented "projection",
            unimplemented "import_stl"-}
            -- rotateExtrude
            ] <* whitespace
    *<|>  -- Non suite s. Semicolon needed...
        whitespace *> tryMany [
            echo,
            assignment,
            include--,
            --use
            ] <* charSep ';'
    *<|> whitespace *> userModule <* whitespace

{-
-- | A suite of s!
--   What's a suite? Consider:
--
--      union() {
--         sphere(3);
--      }
--
--  The suite was in the braces ({}). Similarily, the
--  following has the same suite:
--
--      union() sphere(3);
--
--  We consider it to be a list of s which
--  are in tern StatementI s.
--  So this parses them.
-}
suite :: Parser [StatementI]
suite = (fmap return computation
         <|> braces (many (try computation))) <?> " suite"

throwAway :: Parser StatementI
throwAway = withPos $ (lexeme (oneOf "%*") *> computation) >> (return DoNothing)

-- An included ! Basically, inject another openscad file here...
include :: Parser StatementI
include = (withPos $ do
    injectVals <-  (symbol "include" >> return True )
               <|> (symbol "use"     >> return False)
    filename <- angleBrackets (many (noneOf "<> "))
    return $ Include filename injectVals
    ) <?> "include "

-- | An assignment  (parser)
assignment :: Parser StatementI
assignment = ("assignment " ?:) $
    withPos $ ((:=) <$> patternMatcher <* (charSep '=') <*> expr0)
    *<|> do
        varSymb <- (string "function" >> space >> whitespace >> variableSymb) 
                   *<|> variableSymb
        argVars <- parens (sepBy patternMatcher (charSep ','))
        _ <- charSep '='
        valExpr <- expr0
        return $ Name varSymb := LamE argVars valExpr

-- | An echo  (parser)
echo :: Parser StatementI
echo = withPos $ Echo <$> ((symbol "echo") *> (parens (expr0 `sepBy` (charSep ','))))

ifStatementI :: Parser StatementI
ifStatementI = 
    "if " ?: (withPos $
        If <$> ((symbol "if") *> (parens expr0)) <*> suite <*>
          (((symbol "else") *> suite) *<|> (return [])))

destructuringAssignment :: Parser (Pattern, Expr)
destructuringAssignment = do
  pattern <- patternMatcher
  _ <- charSep '='
  vexpr <- expr0
  return $ (pattern, vexpr)

forStatementI :: Parser StatementI
forStatementI =
    "for " ?: (withPos $ do
        -- a for loop is of the form:
        --      for ( vsymb = vexpr   ) loops
        -- eg.  for ( a     = [1,2,3] ) {echo(a);   echo "lol";}
        -- eg.  for ( [a,b] = [[1,2]] ) {echo(a+b); echo "lol";}
        _ <- symbol "for"
        (pattern, vexpr) <- parens destructuringAssignment
        loopContent <- suite
        return $ For pattern vexpr loopContent)

userModule :: Parser StatementI
userModule =
  withPos (ModuleCall <$> variableSymb <*> moduleArgsUnit
           <*> (suite *<|> (charSep ';' >> return [])))

userModuleDeclaration :: Parser StatementI
userModuleDeclaration =
  withPos (NewModule <$> ((symbol "module") *> variableSymb)
           <*> moduleArgsUnitDecl <*> suite)

----------------------

moduleArgsUnit :: Parser [(Maybe String, Expr)]
moduleArgsUnit =
    parens $ sepBy ( 
        do
            -- eg. a = 12
            symb <- variableSymb
            _ <- charSep '='
            expr <- expr0
            return $ (Just symb, expr)
        *<|> do
            -- eg. a(x,y) = 12
            symb <- variableSymb
            argVars <- parens (sepBy variableSymb (try $ charSep ','))
            _ <- charSep '='
            expr <- expr0
            return $ (Just symb, LamE (map Name argVars) expr)
        *<|> do
            -- eg. 12
            expr <- expr0
            return (Nothing, expr)
        ) (try $ charSep ',')

moduleArgsUnitDecl ::  Parser [(String, Maybe Expr)]
moduleArgsUnitDecl =
    parens $ sepBy (
        do
            symb <- variableSymb
            _ <- charSep '='
            expr <- expr0
            return (symb, Just expr)
        *<|> do
            symb <- variableSymb
            argVars <- parens (sepBy variableSymb (try $ charSep ','))
            _ <- charSep '='
            expr <- expr0
            return (symb, Just expr)
        *<|> do
            symb <- variableSymb
            return (symb, Nothing)
        ) (try $ charSep ',')

withPos :: Parser (Statement StatementI) -> Parser StatementI
withPos p = StatementI <$> lineNumber <*> p
  where lineNumber = fmap sourceLine getPosition
