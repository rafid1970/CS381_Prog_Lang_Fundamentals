module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


{-
-- | Environment queries.
data Test = Not    Test   -- boolean negation
          | Facing Card   -- am I facing the given cardinal direction?
          | Clear  Dir    -- can I move in the given relative direction?
          | Beeper        -- is there a beeper here?
          | Empty         -- is my beeper bag empty?
  deriving (Eq,Show)
-}
-- ^^^ Essentially test these
-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) world robit = not(test t world  robit) -- flip boolean
test (Facing card) world robit = card == getFacing robit -- helper function from KarelState.hs
test (Clear dir) world robit = isClear (relativePos dir robit) world  -- change next step to Pos and pass that to clear
test (Beeper) world robit = hasBeeper (getPos robit) world  -- get current position and check for beeper
test (Empty) world robit = isEmpty robit



{--
- | Statements.
data Stmt = Shutdown                 -- end the program
          | Move                     -- move forward
          | PickBeeper               -- take a beeper
          | PutBeeper                -- leave a beeper
          | Turn    Dir              -- rotate in place
          | Call    Macro            -- invoke a macro
          | Iterate Int  Stmt        -- fixed repetition loop
          | If      Test Stmt Stmt   -- conditional branch
          | While   Test Stmt        -- conditional loop
          | Block   [Stmt]           -- statement block
  deriving (Eq,Show)
-}
-- ^^ Build these
-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt Move       defs world robit = let pos = relativePos Front robit -- assign pos the position directly infront of robit
                                    in if (isClear pos world == True)
                                      then OK world (setPos pos robit)
                                      else Error ("Blocked at: " ++ show pos)

stmt PickBeeper  _ w r           = let p = getPos r
                                      in if (hasBeeper p w == True)
                                          then OK (decBeeper p w) (incBag r)
                                          else Error ("No beeper to pick at: " ++ show p)

stmt PutBeeper _ world robit     = let p = getPos robit
                                      in if (isEmpty robit == True)
                                            then Error ("No beeper to put.")
                                            else OK (incBeeper p world )(decBag robit)

stmt (Turn dir) _ world robit    = let card = getFacing robit in
                                      OK world (setFacing (cardTurn dir card)  robit) -- get current dir, change to new dir, return result

stmt (Call macro) defs world robit  = case lookup macro defs of -- evaluate macro in defs list
                                        (Just body) -> stmt body defs world robit --case of the body returning, call
                                        _ -> Error ("Undefined macro: " ++ macro) -- case of nothing returning

stmt (Iterate 0 eval) defs world robit = OK world robit -- base case

stmt (Iterate num eval) defs world robit = if num /= 0 then
                                            case stmt eval defs world robit of
                                              OK world' robit' -> stmt (Iterate (num - 1) eval) defs world' robit'
                                              Done robit' -> Done robit'
                                              Error eval' -> Error eval'
                                           else Error ("Fail in the loop") -- should never be ran, needed for syntax

stmt (If try passed failed) defs world robit = if (test try world robit == True)
                                                then stmt passed defs world robit
                                                else stmt failed defs world robit

stmt (While crit passed) defs world robit = if (test crit world robit == True) then
                                               case stmt passed defs world robit of
                                                 (OK world' robit') -> stmt (While crit passed) defs world' robit'
                                                 (Done robit') -> Done robit'
                                                 (Error err) -> Error err
                                            else OK world robit

stmt (Block []) defs world robit = OK world robit --Base case
stmt (Block (x:xs)) defs world robit = case stmt x defs world robit of
                                       (OK world' robit') -> stmt (Block xs) defs world' robit' -- evaluate statement
                                       (Done robit') -> Done robit' -- Program ends
                                       (Error err) -> Error err -- pass along error

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
