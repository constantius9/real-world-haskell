import Data.List

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

asInt_fold :: String -> Int
asInt_fold =
	undefined

test_AsInt1 =
    asInt_fold "101"
    @?= 101

test_AsInt2 =
    asInt_fold "-31337"
    @?= (-31337)

test_AsInt3 =
    asInt_fold "1798"
    @?= 1798

main = defaultMain tests

tests = [
    testGroup "As Int" [
        testCase "Case 1"
            test_AsInt1,
        testCase "Case 2"
            test_AsInt2,
        testCase "Case 3"
            test_AsInt3
        ]
    ]
