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
-- ^^ Test these
-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt _ _ _ _ = undefined

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
