{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser where

import AST

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char hiding (space, hspace)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Control.Monad.State
import Control.Lens
import Data.List (singleton)

newtype ParserST = ParserST { _indents :: [Pos] }
makeLenses ''ParserST

type Parser = ParsecT Void Text (State ParserST)

spaceOf :: Parser () -> Parser ()
spaceOf p = L.space p
    (L.skipLineComment "--")
    (L.skipBlockComment "/-" "-/")

space, hspace :: Parser ()
space = spaceOf space1
hspace = spaceOf hspace1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: Text -> Parser Text
symbol = L.symbol hspace

parens, braces, angles, brackets, quotes :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")
angles   = between (symbol "<") (symbol ">")
brackets = between (symbol "[") (symbol "]")
quotes   = between (char '\'') (symbol "'")

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf ("-_'<>?" :: String)

-- For testing in REPL
run :: Parser a -> Text -> a
run p i = either (error . errorBundlePretty) id
    $ evalState (runParserT p "" i) (ParserST [pos1])

parseIdent :: Parser Text
parseIdent = fmap T.pack . lexeme $ (:) <$> letterChar <*> many identChar

parseIndent :: Parser ()
parseIndent = do
    ref <- use indents <&> head
    new <- L.indentGuard hspace GT ref
    indents %= (new :)

parseBlockOf :: Parser a -> Parser [a]
parseBlockOf p = do
    x <- parseIndent *> p <* eol
    ref <- use indents <&> head
    let line = L.indentGuard hspace EQ ref *> p <* eol
    xs <- many line
    indents %= tail
    return (x:xs)

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
    , try $ VarPat <$> parseIdent <* notFollowedBy (char '=')
    , BindPat <$> parseIdent <* symbol "=" <*> parsePatternTerm ]

-- TODO: Line folding? User defined operators? Macros?
parseExpr :: Parser Expr
parseExpr = makeExprParser parseExprTerm
    [ [ Prefix $ fn1 "neg" <$ char '-' ]
    , [ InfixR $ fn2 "pow" <$ symbol "^" ]
    , [ InfixL $ fn2 "mult" <$ symbol "*"
      , InfixL $ fn2 "div" <$ symbol "/" ]
    , [ InfixL $ fn2 "add" <$ symbol "+"
      , InfixL $ fn2 "sub" <$ symbol "-" ]
    , [ InfixR $ fn2 "call" <$ symbol "$" ] ]
    where
        fn1 name expr = Apply (Var name) [expr]
        fn2 name lhs rhs = Apply (Var name) [lhs, rhs]

parseExprTerm :: Parser Expr
parseExprTerm = choice
    [ Lit <$> parseLiteral
    , try $ Apply . Var <$> parseIdent <*> some parseExprTerm
    , Var <$> parseIdent
    , Array <$> brackets (parseExpr `sepBy` symbol ",")
    , parens parseExpr
    , parseIfElse ]

parseIfElse :: Parser Expr
parseIfElse = IfElse
    <$> (symbol "if" *> parseExprTerm)
    <*> parseBlock
    <*> (symbol "else" *> parseBlock)

parseStmt :: Parser Stmt
parseStmt = choice
    [ try $ DeclTy <$> parseIdent <* symbol ":" <*> parseTy
    , try $ DeclVar <$> parseIdent <* symbol "=" <*> parseBlock
    , DoExpr <$> parseExpr ]

parseBlock :: Parser Block
parseBlock = Block <$> choice
    [ eol *> parseBlockOf parseStmt
    , singleton . DoExpr <$> parseExpr ]

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
    [ AnyTy <$ symbol "any"
    , EmptyTy <$ symbol "void"
    , IntTy <$ symbol "int"
    , FloatTy <$ symbol "float"
    , CharTy <$ symbol "char"
    , StrTy <$ symbol "str"
    , SymTy <$ symbol "sym"
    , parens parseTy
    , ArrayTy <$> brackets parseTy
    , SingTy <$> parseLiteral
    , try $ ApplyTy <$> parseIdent <*> some parseTyTerm
    , VarTy <$> parseIdent
    , parseForallTy ]
    where
        parseForallTy :: Parser Ty
        parseForallTy = ForallTy <$> parseForall <*> parseTy
        parseForall = between (symbol "\\") (symbol ".") $ some parseArg
        parseArg = parsePair <|> (, AnyTy) <$> parseIdent
        parsePair = parens $ (,) <$> parseIdent <* symbol ":" <*> parseTy
