-- | A simple expression language with two types.
module IntBool where


--  Syntax of the IntBool language:
--
--  int  ::=  (any integer)
--  bool ::=  true  |  false
--
--  exp  ::=  int              integer literal
--        |   exp + exp        integer addition
--        |   exp = exp        check whether two values are equal
--        |   !exp             boolean negation


data Expr = Lit Int
          | Add Expr Expr
          | Equ Expr Expr
          | Not Expr
    deriving(Eq,Show)


-- 2. Identify/define the semantic domain for this language
--   * what types of values can we have?
--   * how can we express this in Haskell?

-- We need an Int and a bool
data Value = I Int
           | B Bool
           | TypeError
    deriving(Eq,Show)

-- Define the semantic function / denotational semantics
sem :: Expr -> Value
sem (Lit i) = I i
sem (Add l r) = case (sem l, sem r) of
              (I i, I j) -> I (i+j)
              _          -> TypeError
sem (Equ l r) = case (sem l, sem r) of
              (I i, I j) -> B (i == j)
              (B a, B b) -> B (a == b)
              _          -> TypeError
sem (Not e)   = case (sem e) of
                (B b) -> B (not b)
                _     -> TypeError


data Type = Tint | Tbool | Terr
-- 2. Define the typing relation. (Maps AST to types)
typeOf :: Expr -> Type
typeOf (Lit i) = Tint
typeOf (Add l r) = case (typeOf l, typeOf r) of
                  (Tint, Tint) -> Tint
                  _            -> Terr
typeOf (Equ l r) = case (typeOf l, typeOf r) of
                    (Tint, Tint) -> Tbool
                    (Tbool, Tbool) -> Tbool
                    _             -> Terr
typeOf (Not e)  = case sem e of
                    (Tbool) -> Tbool
                    _       -> Terr
