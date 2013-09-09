import Prelude hiding (Left, Right)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

data Direction = Left
               | Right
               | Straight
                 deriving (Show, Eq)

data Point2D = Point2D { x :: Integer
                       , y :: Integer
                       } deriving (Show, Eq, Ord)

direction :: Point2D -> Point2D -> Point2D -> Direction
direction a b c =
    let x1  = x a
        x2  = x b
        x3  = x c
        y1  = y a
        y2  = y b
        y3  = y c
        x1' = fromIntegral x1
        x2' = fromIntegral x2
        x3' = fromIntegral x3
        y1' = fromIntegral y1
        y2' = fromIntegral y2
        y3' = fromIntegral y3
        k   = (y1' - l) / x1'
        l   = (x1' * y2' - x2' * y1') / (x1' - x2')
        s   = k * (fromIntegral x3) + l
    in case compare s (fromIntegral y3) of
        LT    -> Right
        GT    -> Left
        EQ    -> Straight


test_Left =
    direction (Point2D {x=0, y=0}) (Point2D {x=1, y=1}) (Point2D {x=2, y=3})
    @?= Left

test_Straight =
    direction (Point2D {x=0, y=0}) (Point2D {x=1, y=1}) (Point2D {x=2, y=2})
    @?= Straight

test_Right =
    direction (Point2D {x=0, y=0}) (Point2D {x=1, y=1}) (Point2D {x=2, y=1})
    @?= Right

main = defaultMain tests

tests = [
    testGroup "Direction" [
        testCase "Direction works right for left turn"
            test_Left,
        testCase "Direction works right for straight line"
            test_Straight,
        testCase "Direction works right for right turn"
            test_Right
        ]
    ]
