{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void Text

space :: Parser ()
space = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/-" "-/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

parens, braces, angles, brackets, quotes, dquotes :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")
angles   = between (symbol "<") (symbol ">")
brackets = between (symbol "[") (symbol "]")
quotes   = between (char '\'') (symbol "'")
dquotes  = between (char '"') (symbol "\"")

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf ("-_+='<>?" :: String)

parseIdent :: Parser Text
parseIdent = fmap T.pack . lexeme $ (:) <$> letterChar <*> many identChar

parseLiteral :: Parser Literal
parseLiteral = choice
    [ try $ FloatLit <$> lexeme L.float
    , IntLit <$> lexeme L.decimal
    , try $ CharLit <$> quotes L.charLiteral
    , SymLit . T.pack <$> parseSym
    , StrLit . T.pack <$> parseString ]
    -- , ArrayLit <$> brackets (parseExpr `sepBy` symbol ",") ]
    where
        parseString = char '"' >> manyTill L.charLiteral (symbol "\"")
        parseSym = lexeme (char '\'' >> some identChar)

parseForall :: Parser [(Ident, Ty)]
parseForall = between (symbol "\\") (symbol ".") $ some parseArg
    where
        parseArg = parsePair <|> (, AnyTy) <$> parseIdent
        parsePair = parens $ (,) <$> parseIdent <* symbol ":" <*> parseTy

parseExpr :: Parser Expr
parseExpr = undefined

parseTy :: Parser Ty
parseTy = makeExprParser parseTyTerm
    [ [ Prefix $ NegTy <$ symbol "~" ]
    , [ InfixL $ ProdTy <$ symbol "*" ]
    , [ InfixL $ DiffTy <$ diff ]
    , [ InfixL $ InterTy <$ symbol "&" ]
    , [ InfixL $ UnionTy <$ symbol "|" ]
    , [ InfixR $ FnTy <$ symbol "->" ] ]
    where diff = try $ symbol "-" <* notFollowedBy (char '>')

parseTyTerm :: Parser Ty
parseTyTerm = choice
    [ AnyTy <$ symbol "Any"
    , EmptyTy <$ symbol "Void"
    , IntTy <$ symbol "Int"
    , FloatTy <$ symbol "Float"
    , CharTy <$ symbol "Char"
    , StrTy <$ symbol "Str"
    , SymTy <$ symbol "Sym"
    , parens parseTy
    , ArrayTy <$> brackets parseTy
    , SingTy <$> parseLiteral
    , try $ ApplyTy <$> parseIdent <*> some parseTyTerm
    , VarTy <$> parseIdent
    , ForallTy <$> parseForall <*> parseTy]
