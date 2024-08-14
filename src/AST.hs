module AST where

import Data.Text (Text)

type Ident = Text

data Literal
    = IntLit Integer
    | FloatLit Double
    | CharLit Char
    | StrLit Text
    | SymLit Text
    | ArrayLit [Literal]
    deriving (Show, Eq)

-- TODO: guard?
data Expr
    = Lit Literal
    | Prod [Expr]
    | Apply Ident [Expr]
    | Fn [Ident] Expr
    | Match Expr [(Pattern, Expr)]
    -- | Guard Expr [(Qualifier, Expr)]
    deriving (Show, Eq)

-- data Qualifier = ???

data Pattern
    = LitPat Literal
    | VarPat Ident
    | BindPat Pattern Ident
    | ProdPat Pattern Pattern
    | OrPat Pattern Pattern
    | TyPat Pattern Ty
    deriving (Show, Eq)

-- TODO: Records, row polymorphism, primitive types
data Ty
    = AnyTy                 -- any
    | EmptyTy               -- void
    | IntTy                 -- int
    | FloatTy               -- float
    | CharTy                -- char
    | StrTy                 -- str
    | SymTy                 -- sym
    | ArrayTy Ty            -- [ty]
    | SingTy Literal        -- e.g. 'ok or 0
    | VarTy Ident           -- e.g. a
    | NegTy Ty              -- ~ty
    | UnionTy Ty Ty         -- ty | ty
    | InterTy Ty Ty         -- ty & ty
    | DiffTy Ty Ty          -- ty - ty
    | ProdTy Ty Ty          -- ty * ty
    | FnTy Ty Ty            -- ty -> ty
    | ApplyTy Ty [Ty]       -- ty a
    | ForallTy [Ident] Ty   -- \a. ty a
    deriving (Show, Eq)
