module BinTree where

import Prelude
-- type declaration
data BinTree a = NullTree 
            | Node (BinTree a) a (BinTree a)
              deriving Show

-- Function to check if a given binary tree is null or not
isnull :: BinTree a -> Bool
isnull NullTree = True
isnull _        = False

-- Tree with one node
singleton :: a -> BinTree a
singleton a = Node NullTree a NullTree

-- Inorder traversal
inorder :: BinTree a -> [a]
inorder NullTree = []
inorder (Node lt a rt) = inorder lt ++ a : inorder rt
{-
The : operator is known as the "cons" operator and is used to prepend a head element to a list. So [] is a list and x:[] is prepending x to the empty list making a the list [x]. If you then cons y:[x] you end up with the list [y, x] which is the same as y:x:[].
The ++ operator is the list concatenation operator which takes two lists as operands and "combine" them into a single list. So if you have the list [x] and the list [y] then you can concatenate them like this: [x]++[y] to get [x, y].
Notice that : takes an element and a list while ++ takes two lists.
The above example clearly shows that ':' has higher precedence than '++'.
-}

-- Function for clockwise rotation of binary tree
clockwise :: BinTree a -> BinTree a
clockwise (Node  (Node t1 b t2) a t) = Node t1 b (Node t2 a t)
clockwise t = t
{-
                a                        b
               / \                      / \
              /   \                    /   \
             /     \    --------->    /     \
            b       t               t1       a
           / \                              / \
          /   \                            /   \
         t1   t2                          t2    t
-}

-- max element of tree
mymaxT :: BinTree Int -> Int
mymaxT NullTree = 0
mymaxT (Node lt x rt) = maximum [mymaxT lt, x, mymaxT rt]

-- to replace every element in agiven tree structure with a particular given element
mywithShape :: BinTree a -> b -> BinTree b
mywithShape NullTree _ = NullTree
mywithShape (Node lt _ rt) b = Node ltb b rtb
                               where
                                    ltb = mywithShape lt b
                                    rtb = mywithShape rt b
{-
                a0                       b
               / \                      / \
              /   \                    /   \
             /     \    --------->    /     \
            a1      a2               b       b
           / \                              / \
          /   \                            /   \
         a3   a4                          b     b
-}                                

