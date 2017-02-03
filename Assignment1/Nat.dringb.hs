module Nat where

-- Author: Brandon Dring
-- ONID ID: 932232885
import Prelude hiding (Enum(..), sum)

{-
For the purposes of this assignment, you should implement the required mathematical operations by pattern matching on and manipulating the unary representation directly. That is, you should not implement them by first converting the arguments to integers, doing the mathematical operation on integers, and then converting back to the unary representation.

-}

--
-- * Part 1: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--
--   >>> pred Zero
--   Zero
--
--   >>> pred three
--   Succ (Succ Zero)
--
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ Zero) = Zero
pred (Succ i) = Succ(pred i)


-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero i
  | i == Zero = True
  | i /= Zero = False



-- | Convert a natural number to an integer.
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ i) = (toInt i) + 1





-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--
add :: Nat -> Nat -> Nat
add Zero   Zero = Zero
add Zero   (Succ i) =  Succ i
add (Succ i) Zero = Succ i
add (Succ i) (Succ j) = Succ(Succ(add i j))



-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub Zero Zero = Zero
sub Zero (Succ i) = Zero
sub (Succ i) Zero =  Succ i
sub (Succ i) (Succ j) = sub i j





-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--

gt :: Nat -> Nat -> Bool
gt Zero Zero = False
gt Zero (Succ i) = False
gt (Succ i) Zero = True
gt (Succ i) (Succ j) = gt i j

{--}

-- | Multiply two natural numbers.
--
--    mult two Zero
--   Zero
--
--    mult Zero three
--   Zero
--
--    toInt (mult two three)
--   6
--
--    toInt (mult three two)
--   6
--

mult :: Nat -> Nat -> Nat
mult Zero Zero = Zero
mult Zero (Succ i) = Zero
mult (Succ i)    Zero = Zero
mult (Succ i) j = add (mult i j) j


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum [] = Zero
sum (h:t) = add h (sum t)


{-  THIS IS EXTRA CREDIT -}
-- I did it :-)
-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat]
odds = one : map(add two) odds
