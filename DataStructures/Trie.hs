module Trie where

import Prelude hiding (lookup)
import qualified Data.Map as M

data Trie k v = Node (Maybe v) (M.Map k (Trie k v))
  deriving (Show)

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup []     (Node v _)  = v
lookup (k:ks) (Node _ cs) = case M.lookup k cs of
                              Nothing -> Nothing
                              Just t -> lookup ks t

empty :: Trie k v
empty = Node Nothing M.empty

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert []     v (Node _ m) = Node  (Just v) m
insert (k:ks) v (Node v' cs) = case M.lookup k cs of
                                Nothing -> Node v' (M.insert k newTree cs)
                                  where newTree = insert ks v empty
                                Just t -> Node v' (M.insert k newTree cs)
                                  where newTree = insert ks v t

fromList :: Ord k => [([k], v)] -> Trie k v
fromList [] = empty
fromList ((ks,v) : xs) = insert ks v $ fromList xs

type TrieSet k = Trie k ()

fromList' :: Ord k => [[k]] -> TrieSet k
fromList' [] = empty
fromList' (ks:kss) = insert ks () $ fromList' kss

notTrieSetMember :: Ord k => [k] -> TrieSet k -> Bool
notTrieSetMember ks t = case lookup ks t of
                          Nothing -> True
                          Just _ -> False
