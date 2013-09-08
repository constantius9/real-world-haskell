-- file: ch03/Tree.hs
import Data.Maybe

data Tree a = Node (Maybe a) (Maybe (Tree a)) (Maybe (Tree a))
	deriving (Show)
