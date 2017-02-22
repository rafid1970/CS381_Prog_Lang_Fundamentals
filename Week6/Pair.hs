--
-- An expression language with nested pairs
--
module Pair where

-- 1. Define the abstract syntax

-- Concrete syntax:
--
--   e ::= int
--      |  bool
--      |  (e,e)
--      |  fst e
--      |  snd e
--      |  swap e
--
data Expr = LitI Int
          | LitB Bool
          | Pair Expr Expr
          | Fst Expr
          | Snd Expr
          | Swap Expr
  deriving (Eq,Show)


-- 2. Define the semantic domain

data Value = I Int | B Bool | P Value Value
  deriving (Eq,Show)


-- 3. Define the semantic function

sem :: Expr -> Maybe Value
sem (LitI i)   = Just (I i)
sem (LitB b)   = Just (B b)
sem (Pair l r) = case (sem l, sem r) of
                   (Just vl, Just vr) -> Just (P vl vr)
                   _ -> Nothing
sem (Fst e)    = case sem e of
                   Just (P vl _) -> Just vl
                   _ -> Nothing
sem (Snd e)    = case sem e of
                   Just (P _ vr) -> Just vr
                   _ -> Nothing
sem (Swap e)   = case sem e of
                   Just (P vl vr) -> Just (P vr vl)
                   _ -> Nothing


-- 4. Define the structure of types

-- | Valid types for a pair program.
data Type = TInt
          | TBool
          | TPair Type Type
  deriving (Eq,Show)


-- 5. Define the typing relation
typeOf :: Expr -> Maybe Type
typeOf (LitI i)   = Just TInt
typeOf (LitB b)   = Just TBool
typeOf (Pair l r) = case (typeOf l, typeOf r) of
                      (Just tl, Just tr) -> Just (TPair tl tr)
                      _ -> Nothing
typeOf (Fst e)    = case typeOf e of
                      Just (TPair tl tr) -> Just tl
                      _ -> Nothing
typeOf (Snd e)    = case typeOf e of
                      Just (TPair tl tr) -> Just tr
                      _ -> Nothing
typeOf (Swap e)   = case typeOf e of
                      Just (TPair tl tr) -> Just (TPair tr tl)
                      _ -> Nothing


-- Some examples expressions
a = LitI 2               -- 2
b = Pair a (LitB True)   -- (2,True)
c = Swap b               -- swap (2,True)      =>  (True,2)
d = Pair (LitI 3) c      -- (3,swap (2,True))  =>  (3,(True,2))
e = Fst b                -- fst (2,True)       =>  2
f = Snd b                -- snd (2,True)       =>  True
g = Fst (Fst d)          -- fst (fst (3,swap (2,True)))
                         --     =>  fst (fst (3, (True,2))
                         --     =>  fst 3
                         --     =>  type error!
h = Fst (Snd d)          -- fst (snd (3,swap (2,True)))
                         --     =>  fst (snd (3, (True,2))
                         --     =>  fst (True,2)
                         --     =>  True
i = Fst a                -- fst 2              =>  type error!
j = Swap a               -- swap 2             =>  type error!
k = Pair j (LitI 4)      -- (swap 2,4)         =>  type error!
l = Swap k               -- swap (swap 2,4)    =>  type error!

-- | Semantics unit tests.
--
--   >>> sem a
--   Just (I 2)
--
--   >>> sem b
--   Just (P (I 2) (B True))
--
--   >>> sem c
--   Just (P (B True) (I 2))
--
--   >>> sem d
--   Just (P (I 3) (P (B True) (I 2)))
--
--   >>> sem e
--   Just (I 2)
--
--   >>> sem f
--   Just (B True)
--
--   >>> sem g
--   Nothing
--
--   >>> sem h
--   Just (B True)
--
--   >>> all (==Nothing) (map sem [i,j,k,l])
--   True
--
