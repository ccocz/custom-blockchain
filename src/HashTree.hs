{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-
  Merkle Tree implementation
  @author Resul Hangeldiyev (rh402185)

  todo/doubts:
   1) able to access specific member, e.g., x coordinate
   2) which nonce to choose
   3) when there's no right child what should be the hash
   4) simplifying methods with helper functions (preferably go within where)
   5) reducing _ _ _
   6) avoid appending to list (like lab7's reverse)
   7) change to showS concat
-}

module HashTree where

import Hashable32
import Utils

data Tree a = Node Hash a (Tree a) (Tree a) | Empty

leaf :: Hashable a => a -> Tree a
leaf x = Node (hash x) x Empty Empty

twig :: Hashable a => Tree a -> Tree a
twig l@(Node x _ _ _) = Node (hash (x, x)) undefined l Empty

node :: Hashable a => Tree a -> Tree a -> Tree a
node l@(Node x _ _ _) r@(Node a _ _ _) = Node (hash (x, a)) undefined l r

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = Empty
buildTree x = buildTreeLift $ buildLeaves x where
  buildLeaves :: Hashable a => [a] -> [Tree a]
  buildLeaves [] = []
  buildLeaves (x : xs) = leaf x : buildLeaves xs

  buildTreeLift :: Hashable a => [Tree a] -> Tree a
  buildTreeLift x
    | length x == 1 = x !! 0
    | otherwise = buildTreeLift $ mergeNodes x where
      mergeNodes :: Hashable a => [Tree a] -> [Tree a]
      mergeNodes [] = []
      mergeNodes [x] = [twig x]
      mergeNodes (x : xs : xz) = (node x xs) : mergeNodes xz

rep t = replicate (2 * t) ' '

drawTree :: Show a => Tree a -> String
drawTree x = go x 0 where
  go :: Show a => Tree a -> Int -> String
  go Empty _ = ""
  go (Node x y Empty Empty) t = rep t ++ showHash x ++ " " ++ show y ++ "\n"
  go (Node x _ y Empty) t = rep t ++ showHash x ++ " +\n" ++ go y (t + 1)
  go (Node x _ Empty z) t = rep t ++ showHash x ++ " +\n" ++ go z (t + 1)
  go (Node x _ y z) t = rep t ++ showHash x ++ " -\n" ++ go y (t + 1) ++ go z (t + 1)

treeHash :: Tree a -> Hash
treeHash Empty = 0
treeHash (Node x _ _ _) = x

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
   showsPrec d (MerkleProof x y) = showParen (d > 10) $
      showString "MerkleProof "
      . showsPrec 11 x
      . showChar ' '
      . showString (showMerklePath y)

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths x y = go x y [] where
  go :: Hashable a => a -> Tree a -> MerklePath -> [MerklePath]
  go _ Empty _ = []
  go x (Node y _ Empty Empty) p
    | hash x == y = [reverse p]
    | otherwise = []
  go x (Node _ _ l@(Node h1 _ _ _) Empty) p = go x l (Left h1 : p)
  go x (Node _ _ Empty r@(Node h2 _ _ _)) p = go x r (Right h2 : p)
  go x (Node _ _ l@(Node h1 _ _ _) r@(Node h2 _ _ _)) p =
    go x l (Left h2 : p) ++ go x r (Right h1 : p)

showMerklePath :: MerklePath -> String
showMerklePath [] = ""
showMerklePath (Left x : xs) = "<" ++ showHash x ++ showMerklePath xs
showMerklePath (Right x : xs) = ">" ++ showHash x ++ showMerklePath xs

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof _ Empty = Nothing
buildProof x y
  | isJust $ maybeHead $ paths = Just $ MerkleProof x (head paths)
  | otherwise = Nothing
  where paths = merklePaths x y

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof x (MerkleProof a p) = x == foldr (\i j -> getHash i j) (hash a) p where
  getHash (Left v) j = hash(j, v)
  getHash (Right v) j = hash(v, j)