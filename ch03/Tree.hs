-- file: ch03/Tree.hs
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node a b c) = 1 + max (height b) (height c)

test_HeightZero =
    height Empty
    @?= 0

test_HeightOne =
    height (Node "x" Empty Empty)
    @?= 1

test_HeightTwo =
    height (Node "x" Empty (Node "y" Empty Empty))
    @?= 2

main = defaultMain tests

tests = [
    testGroup "Height" [
        testCase "Height works right with no elements"
            test_HeightZero,
        testCase "Height works right with single element"
            test_HeightOne,
        testCase "Height works right with multiple elements"
            test_HeightTwo
        ]
    ]
