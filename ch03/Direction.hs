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

calculate_l :: Point2D -> Point2D -> Double
calculate_l a b =
    let x1  = x a
        x2  = x b
        y1  = y a
        y2  = y b
        x1' = fromIntegral x1
        x2' = fromIntegral x2
        y1' = fromIntegral y1
        y2' = fromIntegral y2
    in  (x1' * y2' - x2' * y1') / (x1' - x2')

calculate_k :: Point2D -> Double -> Double
calculate_k a l =
    let x1  = x a
        y1  = y a
        x1' = fromIntegral x1
        y1' = fromIntegral y1
    in  (y1' - l) / x1'

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
        l   = calculate_l a b
        k   =
            if (x1 /= 0)
                then calculate_k a l
                else calculate_k b l
        s   = k * (fromIntegral x3) + l
    in case compare s (fromIntegral y3) of
        GT    -> Right
        LT    -> Left
        EQ    -> Straight

directionList :: [Point2D] -> [Direction]
directionList (a:b:c:[]) = [direction a b c]
directionList (a:b:c:d) = [direction a b c] ++ directionList ([b,c] ++ d)
directionList _ = undefined

test_Left =
    direction (Point2D {x=0, y=0}) (Point2D {x=1, y=1}) (Point2D {x=2, y=3})
    @?= Left

test_Straight =
    direction (Point2D {x=0, y=0}) (Point2D {x=1, y=1}) (Point2D {x=2, y=2})
    @?= Straight

test_Right =
    direction (Point2D {x=0, y=0}) (Point2D {x=1, y=1}) (Point2D {x=2, y=1})
    @?= Right

test_List3 =
    directionList [
        Point2D {x=0, y=0},
        Point2D {x=1, y=1},
        Point2D {x=2, y=1}
    ]
    @?= [Right]

test_List5 =
    directionList [
        Point2D {x=0, y=0},
        Point2D {x=1, y=1},
        Point2D {x=2, y=1},
        Point2D {x=3, y=2},
        Point2D {x=4, y=3}
    ]
    @?= [Right, Left, Straight]

test_List6 =
    directionList [
        Point2D {x=0, y=0},
        Point2D {x=1, y=1},
        Point2D {x=2, y=1},
        Point2D {x=3, y=1},
        Point2D {x=4, y=4},
        Point2D {x=5, y=4}
    ]
    @?= [Right, Straight, Left, Right]

main = defaultMain tests

tests = [
    testGroup "Direction" [
        testCase "Direction works right for left turn"
            test_Left,
        testCase "Direction works right for straight line"
            test_Straight,
        testCase "Direction works right for right turn"
            test_Right
        ],
    testGroup "Direction List" [
        testCase "Direction List works right for list of 3"
            test_List3,
        testCase "Direction List works right for list of 5"
            test_List5,
        testCase "Direction List works right for list of 6"
            test_List6
        ]
    ]
