{-
  Merkle Tree implementation
  @author Resul Hangeldiyev (rh402185)
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
drawTreePretty (Node x y Empty Empty) t = rep t ++ showHash x ++ " " ++ show y ++ "\n"
drawTreePretty (Node x _ y Empty) t = rep t ++ showHash x ++ " +\n" ++ drawTreePretty y (t + 1)
drawTreePretty (Node x _ Empty z) t = rep t ++ showHash x ++ " +\n" ++ drawTreePretty z (t + 1)
drawTreePretty (Node x _ y z) t = rep t ++ showHash x ++ " -\n" ++ drawTreePretty y (t + 1) ++ drawTreePretty z (t + 1)

rep t = replicate (2 * t) ' '

treeHash :: Tree a -> Hash
treeHash Empty = 0
treeHash (Node x _ _ _) = x

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
   show (MerkleProof x y) = "(MerkleProof " ++ show x ++ " " ++ showMerklePath y ++ ")"

merklePaths :: (Hashable a, Eq a) => a -> Tree a -> [MerklePath]
merklePaths x y = getMerklePaths x y []

getMerklePaths :: (Hashable a, Eq a) => a -> Tree a -> MerklePath -> [MerklePath]
getMerklePaths _ Empty _ = []
getMerklePaths x (Node _ y Empty Empty) p
  | x == y = [p]
  | otherwise = []
getMerklePaths x (Node _ _ l@(Node h1 _ _ _) Empty) p = getMerklePaths x l (p ++ [Left h1])
getMerklePaths x (Node _ _ Empty r@(Node h2 _ _ _)) p = getMerklePaths x r (p ++ [Right h2])
getMerklePaths x (Node _ _ l@(Node h1 _ _ _) r@(Node h2 _ _ _)) p =
  getMerklePaths x l (p ++ [Left h2]) ++ getMerklePaths x r (p ++ [Right h1])

showMerklePath :: MerklePath -> String
showMerklePath [] = ""
showMerklePath (Left x : xs) = "<" ++ showHash x ++ showMerklePath xs
showMerklePath (Right x : xs) = ">" ++ showHash x ++ showMerklePath xs

buildProof :: (Hashable a, Eq a) => a -> Tree a -> Maybe (MerkleProof a)
buildProof _ Empty = Nothing
buildProof x y
  | isJust $ maybeHead $ paths = Just $ MerkleProof x (head paths)
  | otherwise = Nothing
  where paths = merklePaths x y

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof x (MerkleProof a p) = x == foldr (\i j -> getHash i j) (hash a) p where
  getHash (Left v) j = hash(j, v)
  getHash (Right v) j = hash(v, j)