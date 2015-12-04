module Graphics.Implicit.ExtOpenScad.Parser.Util where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad (void)
import Text.ParserCombinators.Parsec  hiding (State)
import Graphics.Implicit.ExtOpenScad.Definitions

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof


-- white space, including tabs, newlines and comments
whitespace :: Parser ()
whitespace =
  choice [simpleWhiteSpace *> whitespace
         ,lineComment *> whitespace
         ,blockComment *> whitespace
         ,return ()]
  where
    lineComment = try (string "//")
                  *> manyTill anyChar (void (char '\n') <|> eof)
    blockComment = try (string "/*")
                  *> manyTill anyChar (try (string "*/"))
    simpleWhiteSpace = void $ many1 $ oneOf " \t\n\r" 

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

comma :: Parser Char
comma = lexeme $ char ','

charSep :: Char -> Parser ()
charSep = void . lexeme . char

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a
parens = between (lexeme $ char '(') (lexeme $ char ')')

brackets :: Parser a -> Parser a
brackets = between (lexeme $ char '[') (lexeme $ char ']')

braces :: Parser a -> Parser a
braces = between (lexeme $ char '{') (lexeme $ char '}')

angleBrackets :: Parser a -> Parser a
angleBrackets = between (lexeme $ char '<') (lexeme $ char '>')

infixr 1 *<|>
a *<|> b = try a <|> b

infixr 2 ?:
l ?: p = p <?> l

tryMany = (foldl1 (<|>)) . (map try)

variableSymb = lexeme (many1 (noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=") <?> "variable")

patternMatcher :: Parser Pattern
patternMatcher =
    (do
        _ <- lexeme (char '_')
        return Wild
    ) <|> {-( do
        a <- literal
        return $ \obj ->
            if obj == (a undefined)
            then Just (Map.empty)
            else Nothing
    ) <|> -} Name <$> variableSymb
    <|> ListP <$> (brackets (patternMatcher `sepBy` (try $ lexeme (char ','))))
