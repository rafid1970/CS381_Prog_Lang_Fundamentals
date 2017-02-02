{-
e in Expr ::= add ee
          | mul ee
          | neg e
          | i

2 + 3 + 4

   add
  /   \
2    add
    /   \
  3     4

-5 *(6+7)

    mult
  /     \
neg     add
 |     /   \
 5     6     7

 What are the integeres results of evauating the AST's?

    neg
     |
    add       = -8
  /   \
 5     3


     add
    /   \
  neg    3
   |
   5
-- Write stacklang program that
    Checks 3 & 4 are eq
      3 4 eq

    if so, return result of add 5 and 6
      3 4 eq if 5 6 add

    else, return false
      3 4 eq if 5 6 add else false end
      ^^ Concrete syntax
-}

--Grammar for StackLang:
{-
num ::= (any number)
bool ::= `true` | `false`
prog ::= cmd*
cmd ::= num  -- push num on stack
    | bool  -- push bool on stack
    | `add` -- add two nums on top of stack
    | `eq`  --check if top two elements are equal
    | `if` prog `else` prog -- if value on top of stack is true run first program else run second

-}
-- Encode grammar as a set of Haskell data types
import Prelude hiding(Num, EQ)
type Num = Int
type Prog = [Cmd]
data Cmd = PushNum Num
          | PushBool Bool
          | Add
          | EQ
          | IfElse Prog Prog
  deriving(Eq,Show)

-- Write a stack lang program as a Haskell value
--  3 4 add 5 eq

ex1 :: Prog
ex1 = [PushNum 3, PushNum 4, Add, PushNum 5, EQ]


ex2 :: Prog
ex2 = [PushNum 3, PushNum 4, EQ, IfElse [PushNum 5, PushNum 6, Add] [PushBool False]]



-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack
genAdd2 :: Num -> Num -> Prog
genAdd2 x y = [PushNum x, PushNum y, Add, Add]
-- genAdd2 x y = [PushNum y, Add, PUshNum x, Add]



-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that sums them all up.
genSum :: [Num] -> Prog
genSum [] = [PushNum 0]
genSum (x:xs) = genSum xs ++ [PushNum x, Add]


--
-- * Semantics of StackLang (Later!)
--


-- 6. Identify/define a semantics domain for Cmd and
--    for Prog.



-- 7. Define the semantics of a StackLang command (ignore If at first).
cmd = undefined

-- 8. Define the semantics of a StackLang program.
prog = undefined

-- | Run a program on an initially empty stack.
--
--   >>> run ex2
--   Just [Right False]
--
--   >>> run (genSum [1..10])
--   Just [Left 55]
--
--   >>> run [PushN 3, Add, PushN 4]
--   Nothing
--
--run :: Prog -> Maybe Stack
--run p = prog p []
