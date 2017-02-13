module Tree where

-- Author: Brandon Dring
-- ONID ID: 932232885

--
-- * Part 2: Binary trees
--

-- | Integer-labeled binary trees.
data Tree = Node Int Tree Tree   -- ^ Internal nodes
          | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)


-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                    (Leaf 6))
            (Node 7 (Leaf 8) (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
            (Node 8 (Leaf 7) (Leaf 9))


-- | Count nodes in the tree
--
--   >>> counter (Leaf 3)
--   1
--
--   >>> counter (Node 5 (Leaf 6) (Leaf 7))
--   3
--
--   >>> counter t1
--   9
--
--   >>> counter t2
--   9
--
counter :: Tree -> Int
counter (Leaf i) = 1
counter (Node n l r) = counter l + 1 + counter r





-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1
--
leftmost :: Tree -> Int
leftmost (Leaf i)     = i
leftmost (Node _ l _) = leftmost l


-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9
--
rightmost :: Tree -> Int
rightmost (Leaf i) = i
rightmost (Node _ _ r) = rightmost r


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--

maxInt :: Tree -> Int
maxInt (Leaf i) = i
maxInt (Node n r l)
  | n < maxInt l = maxInt l
  | n < maxInt r = maxInt r
  | n > maxInt r = n
  | n > maxInt l = n



-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--
minInt :: Tree -> Int
minInt (Leaf i) = i
minInt (Node n l r)
  | n > minInt r = minInt r
  | n < minInt l = n
  | n > minInt l = minInt l
  | n < minInt r = n




-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--
sumInts :: Tree -> Int
sumInts (Leaf i) = i
sumInts (Node n l r) = sumInts l + sumInts r + n



-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--
preorder :: Tree -> [Int]
preorder (Leaf i) = [i]
preorder (Node n l r) = n:(preorder l) ++ (preorder r)


-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--
inorder :: Tree -> [Int]
inorder (Leaf i) = [i]
inorder (Node n l r) = (inorder l) ++ [n] ++ (inorder r)
--                     ^^ Goes to left ^ middle ^^ right


-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--
isBST :: Tree -> Bool
isBST (Leaf l) = True
isBST (Node n l r) =
  if n > (minInt l)
    then isBST l
    else if n > (minInt r)
      then isBST r
    else False
-- Basically only checks if the nodes are bigger than the leafs under it, which via BST def is true

-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--
inBST :: Int -> Tree -> Bool
inBST i (Leaf l) = if i == l then True else False
inBST i (Node n l r)
  | i == n = True
  | i < n = inBST i l
  | i > n = inBST i r