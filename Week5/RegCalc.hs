-- | A simple calculator language with a single memory register.
module RegCalc where


-- 1. Define the abstract syntax.
data Exp = Lit Int       -- a literal integer
         | Neg Exp       -- integer negation
         | Add Exp Exp   -- integer addition
         | Set Exp       -- save result to register and return it
         | Get           -- return the value of the register
  deriving (Eq,Show)


-- Examples programs:
w = Add (Set (Lit 3)) Get                      -- \s -> (3,6)
x = Add (Set (Lit 3)) (Add Get Get)            -- \s -> (3,9)
y = Add (Set (Lit 3)) (Add Get (Set (Lit 5)))  -- \s -> (5,11)
z = Add (Set (Lit 3)) (Add (Set (Lit 5)) Get)  -- \s -> (5,13)
c = Add (Lit 3) Get                            -- \s -> (s, 3+s)


-- 2. Identify/define the semantic domain for this language.

type Reg = Int
type Domain = Reg -> (Reg,Int)


-- 3. Define the semantic function.
sem :: Exp -> Reg -> (Reg,Int)
sem (Lit i)   = \s -> (s,i)
sem (Neg e)   = \s -> let (s',n) = sem e s in (s', negate n)
sem (Add l r) = \s -> let (sl,n) = sem l s
                          (sr,m) = sem r sl
                      in (sr,n+m)
sem (Set e)   = \s -> let (_,n) = sem e s in (n,n)
sem Get       = \s -> (s,s)

-- Semantics of Neg, using a case expression:
--   \s -> case sem e s of
--           (s',n) -> (s', -n)
