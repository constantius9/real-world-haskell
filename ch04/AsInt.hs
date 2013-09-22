import Data.List

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

isDigit c =
    '0' <= c && c <= '9'

sign x
    | x >= 0    = ( 1)
    | otherwise = (-1)

isNotOverflow x x' =
    sign x == sign x'

convertAdd (Left  errorMessage) _       = Left errorMessage
convertAdd (Right acc)          x@(c:_) =
    if isDigit c
        then
            let digit = (read x :: Int)
                nacc  = acc * 10 + digit
            in  if  isNotOverflow nacc acc
                then Right nacc
                else Left $ "convertAdd: integer overflow: old acc == "
                          ++ show acc ++ " newacc == " ++ show nacc
        else Left $ "convertAdd: not a digit " ++ x

breakAll [] = []
breakAll (h:t) =
    [h] : breakAll t

type ErrorMessage = String

asInt_fold :: String -> Either ErrorMessage Int
asInt_fold "" = Left "empty string to convert"
asInt_fold s@(h:t)
    | h == '-'  = fmap ( * (-1) ) (foldl convertAdd (Right 0) $ breakAll t)
    | otherwise = fmap ( * ( 1) ) (foldl convertAdd (Right 0) $ breakAll s)

test_AsInt1 =
    asInt_fold "101"
    @?= Right 101

test_AsInt2 =
    asInt_fold "-31337"
    @?= Right (-31337)

test_AsInt3 =
    asInt_fold "1798"
    @?= Right 1798

test_AsInt4 =
    asInt_fold "200"
    @?= Right 200

main = defaultMain tests

tests = [
    testGroup "As Int" [
        testCase "Case 1"
            test_AsInt1,
        testCase "Case 2"
            test_AsInt2,
        testCase "Case 3"
            test_AsInt3,
        testCase "Case 4"
            test_AsInt4
        ]
    ]
