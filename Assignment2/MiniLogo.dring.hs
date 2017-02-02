{-
Author: Brandon Dring
OSU ID: 932-232-885

Grammar for MiniLogo:

num 	::= 	(any natural number)
var 	::= 	(any variable Var_Name)
macro 	::= 	(any macro Var_Name)

prog 	::= 	ε   |   cmd ; prog 	sequence of commands

mode 	::= 	down   |   up 	pen status

expr 	::= 	var 	variable reference
	| 	num 	literal number
	| 	expr + expr 	addition expression

cmd 	::= 	pen mode 	change pen mode
	| 	move ( expr , expr ) 	move pen to a new position
	| 	define macro ( var* ) { prog }   	define a macro
	| 	call macro ( expr* ) 	invoke a macro
-}

import Prelude hiding(Num)
import Data.List

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up
          | Down
    deriving(Show)



data Expr = Var_Name Var
          | Literal Num
          | Add Expr Expr
    deriving(Show)

data Cmd = Pen Mode
          | Move Expr Expr
          | Define Macro [Var] Prog
          | Call Macro [Expr]
    deriving(Show)


{-
Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas) draws a line segment from (x1,y1) to (x2,y2)
-------------------------------------------------------
  define line(Start_X,Start_Y,End_X,End_Y){
    Pen Up;
    Move(Start_X,Start_Y);
    Pen Down;
    Move(End_X,End_Y);
    Pen Up;
  }
-}


line :: Cmd
line = Define "line" ["Start_X","Start_Y","End_X","End_Y"]
        [Pen Up, Move (Var_Name "Start_X") (Var_Name "Start_Y"), Pen Down, Move (Var_Name "End_X") (Var_Name "End_Y"), Pen Up]

{-
Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h) that draws a big “X” of width w and height h, starting from position (x,y). Your definition should not contain any move commands.
---------------------------------------------------------

  define nix(Start_X,Start_Y,Width,Height){
    // Draw the '\' part of the X
    line(Start_X, Start_Y + Height, Start_X + Width, Start_Y)
    // Draw the '/' part of the X
    line(Start_X, Start_Y, Start_X + Width, Start_Y + Height)
  }
-}

nix :: Cmd
nix = Define "nix" ["Start_X", "Start_Y", "Width", "Height"]
    [
    --Drawing the '\'
    Call "line"[
    (Var_Name "Start_X"), Add (Var_Name "Start_Y") (Var_Name "Height"),

    Add (Var_Name "Start_X") (Var_Name "Width"), (Var_Name "Start_Y")
      ],
    -- Drawing the '/'
    Call "line" [
    (Var_Name "Start_X"), (Var_Name "Start_Y"),
     Add (Var_Name "Start_X") (Var_Name "Width"), Add (Var_Name "Start_Y") (Var_Name "Height")
      ]
    ]


{-
Define a Haskell function steps :: Int -> Prog that constructs a
 MiniLogo program that draws a staircase of n steps starting from
 (0,0).
-}
steps :: Int -> Prog
steps 0 = [Pen Up, Move (Literal 0) (Literal 0), Pen Down]
--                      ^^ Start at the 0,0 Coordinate
steps s = steps (s - 1) ++ [Move (Literal (s - 1)) (Literal s)] ++ [Move (Literal s) (Literal s)]
--        ^^ Draws the '_' of each step, while each recursive call draws the '|'


{-
  Define a Haskell function macros :: Prog -> [Macro] that returns a
  list of the names of all of the macros that are defined anywhere in
  a given MiniLogo program. Don’t worry about duplicates—if a macro
  is defined more than once, the resulting list may include multiple
  copies of its name.
-}

macros :: Prog -> [Macro]
macros [] = []

macros (x:xs) = if (checkforDef x) == True
   then (pullMacro x) : []
   else macros xs

pullMacro :: Cmd -> Macro
pullMacro (Define m _ _) = m
-- get the name of the macro


-- Check to see if the value
checkforDef :: Cmd -> Bool
checkforDef (Define _ _ _ ) = True
-- if the Cmd is Define ... return true
checkforDef _ = False
-- anything else false

{-
Define a Haskell function pretty :: Prog -> String that pretty-prints
a MiniLogo program. That is, it transforms the abstract syntax (a
Haskell value) into nicely formatted concrete syntax (a string of
characters). Your pretty-printed program should look similar to the
example programs given above; however, for simplicity you will
probably want to print just one command per line.

 Pretty works perfectly besides the pretty[nix], its
because of the call command that is in the middle of all
the expressions. I figured it's not worth the headache of trying
to implement it if I did the E.C. Everytime I try something I get a type error because the function is expecting an expression not Cmd.


-}
pretty :: Prog -> String
pretty [] = []
pretty (x:xs) = parseCmd x ++ pretty xs


-- Need to parse every potential Cmd
parseCmd :: Cmd -> String
-- Here the V is an array of strings, while P is a list of commands
parseCmd (Define m v p) = "Define " ++ m ++ "(" ++ breakArray v ++ ") {" ++ " \n" ++ pretty p ++ "}"
parseCmd (Pen Down) = "\tPen Down;" ++ "\n"
parseCmd (Pen Up) = "\tPen Up;" ++ "\n"
parseCmd (Move x y) = " \tMove (" ++ parseExpr x ++ ") (" ++ parseExpr y ++ "); \n"
parseCmd (Call m e) = "\tCall " ++ m ++ " \n\t[" ++ intercalate "," (map parseExpr e)  ++ "]\n"


-- Throws error when nix gets to Call..

breakExprArray :: [Expr] -> String
breakExprArray (x:xs) = parseExpr x ++ "," ++ breakExprArray xs


parseExpr :: Expr -> String
parseExpr (Var_Name var) = var
parseExpr (Literal num) = show (num)
parseExpr (Add x y) = parseExpr x ++ "+" ++ parseExpr y


breakArray :: [Var] -> String
breakArray [] = []
breakArray (x:xs) =  x ++ ", " ++ breakArray xs

{-
Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions by replacing
any additions of literals with the result. For example, given the expression (2+3)+x, optE should
return the expression 5+x.
---------------------------------------------------------------
  Requires my length Data constructor names.. Sorry about that.
  I.E: optE (Add(Add (Literal 5) (Literal 3)) (Var_Name "x"))
-}
optE :: Expr -> Expr
optE (Add (Literal x) (Literal y)) = (handleAdd x y)
optE (Add(Add (Literal x) (Literal y)) (Var_Name z)) = Add (handleAdd x y) (Var_Name z)
optE (Add(Add (Literal x) (Literal y)) (Literal z)) = Add (handleAdd x y) (Literal z)



handleAdd :: Num -> Num -> Expr
handleAdd x y = Literal (x + y)


{-
Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions contained in a
given program using optE.
-----------------------------------------------
E.G:
  optP [Move (Add (Literal 3) (Literal 2)) (Literal 2), Move (Add(Literal 3) (Literal 2)) (Literal 2)]
                              OR
  optP [Move (Add (Literal 3) (Literal 2)) (Literal 2), Move (Add(Literal 3) (Literal 2)) (Var_Name "x")]
-}

optP :: Prog -> Prog
optP [] = []
optP (x:xs) = if (checkForAdd x) == True
            then getExpr x : optP xs
            else optP xs


checkForAdd :: Cmd -> Bool
checkForAdd (Move(Add x y) _) = True
checkForAdd _ = False

getExpr :: Cmd -> Cmd
getExpr (Move(Add (Literal x) (Literal y)) (Literal z)) = addBack (optE (Add(Add(Literal x) (Literal y)) (Literal z)))

getExpr (Move(Add (Literal x) (Literal y)) (Var_Name z)) = addBack (optE (Add(Add(Literal x) (Literal y)) (Var_Name z)))


addBack :: Expr -> Cmd
addBack (Add (Literal x) (Literal z)) = Move (Literal x) (Literal z)
addBack (Add (Literal x) (Var_Name z)) = Move (Literal x) (Var_Name z)
