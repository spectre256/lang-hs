{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Function (on)
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

parens, braces, angles, brackets, quotes :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")
angles   = between (symbol "<") (symbol ">")
brackets = between (symbol "[") (symbol "]")
quotes   = between (char '\'') (symbol "'")

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
    where
        parseString = char '"' >> manyTill L.charLiteral (symbol "\"")
        parseSym = lexeme (char '\'' >> some identChar)

parsePattern :: Parser Pattern
parsePattern = makeExprParser parsePatternTerm
    [ [ Postfix $ flip TyPat <$> (symbol ":" *> parseTyTerm) ]
    , [ InfixL $ ProdPat <$ symbol "," ]
    , [ InfixL $ OrPat <$ symbol "|" ] ]

parsePatternTerm :: Parser Pattern
parsePatternTerm = choice
    [ EmptyPat <$ symbol "_"
    , LitPat <$> parseLiteral
    , parens parsePattern
    , try $ VarPat <$> parseIdent <* notFollowedBy (char '@')
    , BindPat <$> parseIdent <* symbol "@" <*> parsePatternTerm ]

parseExpr :: Parser Expr
parseExpr = undefined

parseExprTerm :: Parser Expr
parseExprTerm = choice
    [ Lit <$> parseLiteral
    , Var <$> parseIdent
    , Array <$> brackets (parseExpr `sepBy` symbol ",") ]
    -- ,  ]

parseTernary :: Parser Expr
parseTernary = do
    _ <- symbol "if"
    cond <- parseExpr
    _ <- symbol "then"
    iftrue <- parseExpr
    _ <- symbol "else"
    iffalse <- parseExpr
    return $ IfThenElse cond (Block [] iftrue) (Block [] iffalse)

parseTy :: Parser Ty
parseTy = makeExprParser parseTyTerm
    [ [ Prefix $ NegTy <$ symbol "~" ]
    , [ InfixL $ ProdTy <$ symbol "*" ]
    , [ InfixL $ DiffTy <$ symbolNFBy "-" '>' ]
    , [ InfixL $ InterTy <$ symbol "&" ]
    , [ InfixL $ UnionTy <$ symbol "|" ]
    , [ InfixR $ FnTy <$ symbol "->" ] ]
    where symbolNFBy s c = try $ symbol s <* notFollowedBy (char c)

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
    , parseForallTy ]

parseForallTy :: Parser Ty
parseForallTy = ForallTy <$> parseForall <*> parseTy
    where
        parseForall = between (symbol "\\") (symbol ".") $ some parseArg
        parseArg = parsePair <|> (, AnyTy) <$> parseIdent
        parsePair = parens $ (,) <$> parseIdent <* symbol ":" <*> parseTy
