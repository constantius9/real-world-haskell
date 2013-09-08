-- file: ch03/Intersperse.hs
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

intersperse :: a -> [[a]] -> [a]
intersperse _ []  = []
intersperse _ [x] = x
intersperse s xs  = head xs ++ [s] ++ intersperse s (tail xs)

test_NoElements =
    intersperse ',' []
    @?= ""

test_SingleElement =
    intersperse ',' ["foo"]
    @?= "foo"

test_MultipleElements =
    intersperse ',' ["foo","bar","baz","quux"]
    @?= "foo,bar,baz,quux"

main = defaultMain tests

tests = [
    testGroup "Intersperse" [
        testCase "Intersperse works right with no elements"
            test_NoElements,
        testCase "Intersperse works right with single element"
            test_SingleElement,
        testCase "Intersperse works right with multiple elements"
            test_MultipleElements
        ]
    ]
