type Pos = (Int, Int)

data Move = Jump Pos
          | GoUp Int
          | GoRight Int
          | Seq Move Move
     deriving (Eq,Show)


sem :: Move -> Pos -> Pos
sem (Jump (x,y)) (a,b) = (x,y)
