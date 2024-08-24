{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- Much of this code is a direct translation of the psuedocode found in the
-- article https://pnwamk.github.io/sst-tutorial/

-- TODO: Handle singleton types; implement parametric polymorphism, records, and row polymorphism

module TypeChecker
    ( (<:)
    , isSubtype
    , proj
    , domain
    , apply )
    where

import Prelude hiding (any, null)
import qualified Data.HashSet as S
import GHC.Generics (Generic)
import Data.Hashable
import Data.Function (on)


infix 5 <:
(<:) :: DNF -> DNF -> Bool
(<:) = isSubtype

isSubtype :: DNF -> DNF -> Bool
isSubtype s t = null $ s `diff` t


class Set a where
    any :: a
    empty :: a
    comp :: a -> a
    comp = diff any
    union :: a -> a -> a
    inter :: a -> a -> a
    diff :: a -> a -> a
    null :: a -> Bool


-- A type in Disjunctive Normal Form
data DNF = DNF
    { base :: BaseDNF
    , prod :: BDD 'ProdTy
    , fn :: BDD 'FnTy}
    deriving (Show, Eq, Generic)

instance Hashable DNF

instance Ord DNF where
    compare = compare `on` hash

instance Set DNF where
    any = DNF any any any
    empty = DNF empty empty empty

    union (DNF b1 p1 f1) (DNF b2 p2 f2) = DNF (union b1 b2) (union p1 p2) (union f1 f2)
    inter (DNF b1 p1 f1) (DNF b2 p2 f2) = DNF (inter b1 b2) (inter p1 p2) (inter f1 f2)
    diff (DNF b1 p1 f1) (DNF b2 p2 f2) = DNF (diff b1 b2) (diff p1 p2) (diff f1 f2)
    null (DNF b p f) = null b && null p && null f


data BaseDNF = BaseDNF
    { polarity :: Polarity
    , types :: S.HashSet BaseTy }
    deriving (Show, Eq, Generic)

instance Hashable BaseDNF

instance Set BaseDNF where
    any = BaseDNF Neg S.empty
    empty = BaseDNF Pos S.empty

    comp (BaseDNF pol b) = BaseDNF (inv pol) b

    union (BaseDNF Pos b1) (BaseDNF Pos b2) = BaseDNF Pos (S.union b1 b2)
    union (BaseDNF Neg b1) (BaseDNF Neg b2) = BaseDNF Neg (S.intersection b1 b2)
    union (BaseDNF Pos b1) (BaseDNF Neg b2) = BaseDNF Neg (S.difference b2 b1)
    union (BaseDNF Neg b1) (BaseDNF Pos b2) = BaseDNF Neg (S.difference b1 b2)

    inter (BaseDNF Pos b1) (BaseDNF Pos b2) = BaseDNF Pos (S.intersection b1 b2)
    inter (BaseDNF Neg b1) (BaseDNF Neg b2) = BaseDNF Neg (S.union b1 b2)
    inter (BaseDNF Pos b1) (BaseDNF Neg b2) = BaseDNF Pos (S.difference b1 b2)
    inter (BaseDNF Neg b1) (BaseDNF Pos b2) = BaseDNF Pos (S.difference b2 b1)

    diff (BaseDNF Pos b1) (BaseDNF Pos b2) = BaseDNF Pos (S.difference b1 b2)
    diff (BaseDNF Neg b1) (BaseDNF Neg b2) = BaseDNF Pos (S.difference b2 b1)
    diff (BaseDNF Pos b1) (BaseDNF Neg b2) = BaseDNF Pos (S.intersection b1 b2)
    diff (BaseDNF Neg b1) (BaseDNF Pos b2) = BaseDNF Neg (S.union b1 b2)

    null (BaseDNF Pos set) = S.null set


data Polarity = Pos | Neg deriving (Show, Eq, Generic)

instance Hashable Polarity

inv :: Polarity -> Polarity
inv Pos = Neg
inv Neg = Pos


-- Base (atomic) types
data BaseTy
    = IntTy
    | FloatTy
    | CharTy
    | StrTy
    | SymTy
    deriving (Show, Eq, Generic)

instance Hashable BaseTy

instance Ord BaseTy where
    compare = compare `on` hash


data AtomTy = ProdTy | FnTy

-- Binary Decision Diagram
-- Represents unions and intersections as paths between nodes
-- The left child is the result if the atom type is satisfied
-- The right child is the result if the atom type is not satisfied
-- The type is inhabited if the leaf is Any, uninhabited if it's Empty
data BDD (a :: AtomTy)
    = AnyBDD
    | EmptyBDD
    | BDDNode
        { atom :: (DNF, DNF)
        , left :: BDD a
        , middle :: BDD a
        , right :: BDD a }
    deriving (Show, Eq, Generic)

instance Hashable (BDD a)

instance {-# OVERLAPPABLE #-} Set (BDD a) where
    any = AnyBDD
    empty = EmptyBDD

    union _ AnyBDD = AnyBDD
    union AnyBDD _ = AnyBDD
    union b EmptyBDD = b
    union EmptyBDD b = b
    union b1@(BDDNode a1 l1 m1 r1) b2@(BDDNode a2 l2 m2 r2) =
        case compare a1 a2 of
            LT -> BDDNode a1 l1 (union m1 b2) r1
            GT -> BDDNode a2 l2 (union m2 b1) r2
            EQ -> BDDNode a1 (union l1 l2) (union m1 m2) (union r1 r2)

    inter :: BDD a -> BDD a -> BDD a
    inter b AnyBDD = b
    inter AnyBDD b = b
    inter _ EmptyBDD = EmptyBDD
    inter EmptyBDD _ = EmptyBDD
    inter b1@(BDDNode a1 l1 m1 r1) b2@(BDDNode a2 l2 m2 r2) =
        case compare a1 a2 of
            LT -> BDDNode a1 (inter l1 b2) (inter m1 b2) (inter r1 b2)
            GT -> BDDNode a2 (inter b1 l2) (inter b1 m2) (inter b1 r2)
            EQ -> BDDNode a1
                (inter (union l1 m1) (union l2 m2))
                EmptyBDD
                (inter (union r1 m1) (union r2 m2))

    diff _ AnyBDD = EmptyBDD
    diff AnyBDD _ = EmptyBDD
    diff b EmptyBDD = b
    diff EmptyBDD b = b
    diff b1@(BDDNode a1 l1 m1 r1) b2@(BDDNode a2 l2 m2 r2) =
        case compare a1 a2 of
            LT -> BDDNode a1 (union l1 m1 `diff` b2) EmptyBDD (union r1 m1 `diff` b2)
            GT -> BDDNode a2 (b1 `diff` union l2 m2) EmptyBDD (b1 `diff` union r2 m2)
            EQ -> BDDNode a1 (l1 `diff` l2) (m1 `diff` m2) (r1 `diff` r2)

    comp AnyBDD = EmptyBDD
    comp EmptyBDD = AnyBDD
    comp (BDDNode a EmptyBDD m r) = BDDNode a (comp m) (comp (union m r)) EmptyBDD
    comp (BDDNode a l EmptyBDD r) = BDDNode a (comp l) (comp (union l r)) (comp r)
    comp (BDDNode a l m EmptyBDD) = BDDNode a EmptyBDD (comp (union l m)) (comp m)
    comp (BDDNode a l m r) = BDDNode a (comp (union l m)) EmptyBDD (comp (union m r))

instance Set (BDD 'ProdTy) where
    null b = nullProd b any any []
        where
            nullProd :: BDD 'ProdTy -> DNF -> DNF -> [(DNF, DNF)] -> Bool
            nullProd EmptyBDD _ _ _ = True
            nullProd AnyBDD s1 s2 n = null s1 || null s2 || nullComb s1 s2 n
            nullProd (BDDNode a@(t1, t2) l m r) s1 s2 n
                = nullProd l (inter s1 t1) (inter s2 t2) n
                && nullProd m s1 s2 n
                && nullProd r s1 s2 (a:n)

            nullComb :: DNF -> DNF -> [(DNF, DNF)] -> Bool
            nullComb _ _ [] = False
            nullComb s1 s2 ((t1, t2):n)
                = (s1 <: t1 || nullComb (s1 `diff` t1) s2 n)
                && (s2 <: t2 || nullComb s1 (s2 `diff` t2) n)


instance Set (BDD 'FnTy) where
    null b = nullFn b any [] []
        where
            nullFn :: BDD 'FnTy -> DNF -> [(DNF, DNF)] -> [(DNF, DNF)] -> Bool
            nullFn EmptyBDD _ _ _ = True
            nullFn AnyBDD _ _ [] = False
            nullFn AnyBDD s p ((t1, t2):n)
                = (t1 <: s && nullComb t1 (comp t2) p)
                || nullFn AnyBDD s p n
            nullFn (BDDNode a@(sd, _) l m r) s p n
                = nullFn l (union s sd) (a:p) n
                && nullFn m s p n
                && nullFn r s p (a:n)

            nullComb :: DNF -> DNF -> [(DNF, DNF)] -> Bool
            nullComb _ _ [] = False
            nullComb t1 t2 ((s1, s2):p)
                = (t1 <: s1 || nullComb (t1 `diff` s1) t2 p)
                && (t2 <: comp s2 || nullComb t1 (inter t2 s2) p)


data ProdLoc = Fst | Snd

-- Calculates the projection of a product type
-- i.e. gets its first or second value
proj :: ProdLoc -> DNF -> Maybe DNF
proj i (DNF _ b _)
    | not $ null b = Just $ proj' i b any any []
    | otherwise = Nothing
    where
        proj' :: ProdLoc -> BDD 'ProdTy -> DNF -> DNF -> [(DNF, DNF)] -> DNF
        proj' _ EmptyBDD _ _ _ = any
        proj' _ _ s1 s2 _
            | null s1 || null s2 = any
        proj' i' AnyBDD s1 s2 n = projComb i' s1 s2 n
        proj' i' (BDDNode a@(t1, t2) l m r) s1 s2 n =
            let
                tl = proj' i' l (inter s1 t1) (inter s2 t2) n
                tm = proj' i' m s1 s2 n
                tr = proj' i' r s1 s2 (a:n)
            in tl `union` tm `union` tr

        projComb :: ProdLoc -> DNF -> DNF -> [(DNF, DNF)] -> DNF
        projComb _ s1 s2 _
            | null s1 || null s2 = any
        projComb i' s1 s2 [] = case i' of
            Fst -> s1
            Snd -> s2
        projComb i' s1 s2 ((t1, t2):n)
            = projComb i' (s1 `diff` t1) s2 n `union` projComb i' s1 (s2 `diff` t2) n


-- Calculates the domain of a function type
domain :: DNF -> Maybe DNF
domain (DNF _ _ b)
    | not $ null b = Just $ domain' any b
    | otherwise = Nothing
    where
        domain' :: DNF -> BDD 'FnTy -> DNF
        domain' _ EmptyBDD = any
        domain' t AnyBDD = t
        domain' t (BDDNode (s1, _) l m r) =
            let
                tl = domain' (union t s1) l
                tm = domain' t m
                tr = domain' t r
            in tl `inter` tm `inter` tr


-- Function application
apply :: DNF -> DNF -> Maybe DNF
apply tf@(DNF _ _ b) ta
    | Just td <- domain tf, ta <: td = Just $ apply' ta any b
    | otherwise = Nothing
    where
        apply' :: DNF -> DNF -> BDD 'FnTy -> DNF
        apply' _ _ EmptyBDD = any
        apply' ta' t _
            | null ta' || null t = any
        apply' _ t AnyBDD = t
        apply' ta' t (BDDNode (s1, s2) l m r) =
            let
                tl1 = apply' ta' (inter t s2) l
                tl2 = apply' (ta' `diff` s1) t l
                tm = apply' ta' t m
                tr = apply' ta' t r
            in tl1 `union` tl2 `union` tm `union` tr
