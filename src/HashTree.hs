{-
  Merkle Tree implementation
  @author Resul Hangeldiyev (rh402185)
-}

module HashTree where

import Hashable32

data Tree a = Node Hash (Tree a) (Tree a) | Empty

leaf :: Hashable a => a -> Tree a
leaf x = Node (hash x) Empty Empty

twig :: Hashable a => Tree a -> Tree a
twig l@(Node x y z) = Node (hash (x, x)) l Empty

node :: Hashable a => Tree a -> Tree a -> Tree a
node l@(Node x y z) r@(Node a b c) = Node (hash (x, a)) l r

buildTree :: Hashable a => [a] -> Tree a
buildTree x = buildTreeLift $ buildLeaves x

buildLeaves :: Hashable a => [a] -> [Tree a]
buildLeaves [] = []
buildLeaves (x : xs) = leaf x : buildLeaves xs

buildTreeLift :: Hashable a => [Tree a] -> Tree a
buildTreeLift x
  | length x == 1 = x !! 0
  | otherwise = buildTreeLift $ mergeNodes x

mergeNodes :: Hashable a => [Tree a] -> [Tree a]
mergeNodes [] = []
mergeNodes [x] = [twig x]
mergeNodes (x : xs : xz) = (node x xs) : mergeNodes xz

drawTree :: Show a => Tree a -> String
drawTree x = drawTreePretty x 0

drawTreePretty :: Show a => Tree a -> Int -> String
drawTreePretty Empty _ = ""
drawTreePretty (Node x Empty Empty) t = rep t ++ showHash x ++ "\n"
drawTreePretty (Node x y Empty) t = rep t ++ showHash x ++ " +\n" ++ drawTreePretty y (t + 1)
drawTreePretty (Node x Empty z) t = rep t ++ showHash x ++ " +\n" ++ drawTreePretty z (t + 1)
drawTreePretty (Node x y z) t = rep t ++ showHash x ++ " -\n" ++ drawTreePretty y (t + 1) ++ drawTreePretty z (t + 1)

rep t = replicate t '\t'

treeHash :: Tree a -> Hash
treeHash Empty = 0
treeHash (Node x y z) = x