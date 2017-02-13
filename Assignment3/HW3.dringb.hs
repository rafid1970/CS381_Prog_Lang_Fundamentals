module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd com (mode,(x,y)) = case (com, mode) of --evaluate what com and mode are
                        (Pen mode, Up) -> ((mode,(x,y)),Nothing)
                        (Pen mode, Down) -> ((mode,(x,y)),Nothing)
                        (Move x y, Up) -> ((Up,(x,y)),Nothing)
                        (Move x2 y2, Down) -> ((Down,(x2,y2)), Just ((x,y),(x2,y2)))

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] (mode,point) = ((mode,point),[]) --(m,p) == Mode & Point of the state that is passed in
prog (x:xs) (mode,point)  = case (cmd x (mode,point)) of --pull out front of Prog and send it to cmd
                        (ret,Nothing) -> prog xs ret -- get to base case
                        (ret, Just line) -> let (s,ls) = prog xs ret in (s,line:ls)




--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = straightLine 0 0 40 30 ++ straightLine 40 30 80 0 ++ straightLine 34 25 35 23 ++ straightLine 35 23 37 25 ++ straightLine 37 25 39 23 ++ straightLine 39 23 41 25 ++ straightLine 41 25 43 23 ++ straightLine 43 23 45 25 ++ bigbox 70 30 ++ box 71 34 ++ box 74 34 ++ straightLine 71 32 71 31 ++ straightLine 71 31 76 31 ++ straightLine 76 31 76 32 ++ straightLine 78 32 79 32 ++  straightLine 78 34 79 34 ++ straightLine 78 36 79 36 ++  straightLine 78 30 79 28 ++  straightLine 78 38 79 39 ++ straightLine 72 29 72 28 ++ straightLine 72 34 72 35 ++ straightLine 74 29 74 28 ++ straightLine 76 29 76 28 ++ straightLine 70 29 68 27  ++ straightLine 69 32 68 32 ++ straightLine 69 35 68 35 ++ straightLine 69 37 67 39 ++ straightLine 72 38 72 39 ++ straightLine 75 38 75 39 ++ straightLine 75 35 75 34 ++ straightLine 0 10 20 25 ++ straightLine 20 25 26 20 ++ straightLine 15 21 16 20 ++ straightLine 16 20 17 21 ++ straightLine 17 21 18 20 ++ straightLine 18 20 19 21 ++ straightLine 19 21 20 20 ++ straightLine 20 20 21 21 ++ straightLine 21 21 22 20 ++ straightLine 22 20 23 21 ++ straightLine 23 21 24 20 ++ straightLine 24 20 25 21 ++ straightLine 80 15 65 25 ++ straightLine 65 25 56 19 ++ straightLine 62 23 63 22 ++ straightLine 63 22 64 23 ++ straightLine 64 23 65 22 ++ straightLine 65 22 66 23 ++ straightLine 66 23 67 22 ++ straightLine 67 22 68 23 ++ trees 3 15 5 ++ straightLine 15 5 15 0 ++ trees 3 25 5 ++ straightLine 25 5 25 0 ++ trees 3 35 5 ++ straightLine 35 5 35 0 ++ trees 3 45 5 ++ straightLine 45 5 45 0 ++ trees 3 55 5 ++ straightLine 55 5 55 0 ++ trees 3 65 5 ++ straightLine 65 5 65 0 

bigbox :: Int -> Int -> Prog
bigbox x y = [Pen Up, Move x y, Pen Down,
           Move (x+7) y, Move (x+7) (y+7), Move x (y+7), Move x y]


trees :: Int -> Int -> Int -> Prog
trees n x y = [Pen Up, Move x y, Pen Down] ++ step n
  where
    step 0 = []
    step n = step (n-1) ++ [Move (x+n-1) (y-n), Move (x-n) (y-n)]

straightLine :: Int -> Int -> Int -> Int-> Prog
straightLine a b c d = [Pen Up, Move a b, Pen Down, Move c d, Pen Down]
