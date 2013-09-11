import Prelude hiding (Left, Right)

import Data.List

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
        s   = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
    in case compare s 0 of
        GT    -> Left
        LT    -> Right
        EQ    -> Straight

directionList :: [Point2D] -> [Direction]
directionList (a:b:c:[]) = [direction a b c]
directionList (a:b:c:d) = [direction a b c] ++ directionList ([b,c] ++ d)

comparePoints :: Point2D -> Point2D -> Ordering
comparePoints a b
  | y1 <  y2             = LT
  | y1 == y2 && x1 <  x2 = LT
  | y1 == y2 && x1 == x2 = EQ
  | y1 == y2 && x1 >  x2 = GT
  | y1 >  y2             = GT
    where x1 = x a
          x2 = x b
          y1 = y a
          y2 = y b

sortPoints :: [Point2D] -> [Point2D]
sortPoints l = sortBy comparePoints l


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

test_SortPoints =
    sortPoints [
        Point2D {x=1, y=3},
        Point2D {x=0, y=0},
        Point2D {x=5, y=4},
        Point2D {x=3, y=1},
        Point2D {x=2, y=2},
        Point2D {x=4, y=5}
    ] @?= [
        Point2D {x=0, y=0},
        Point2D {x=3, y=1},
        Point2D {x=2, y=2},
        Point2D {x=1, y=3},
        Point2D {x=5, y=4},
        Point2D {x=4, y=5}
    ]

test_SortPointsCoincident =
    sortPoints [
        Point2D {x=1, y=1},
        Point2D {x=0, y=0},
        Point2D {x=5, y=4},
        Point2D {x=3, y=1},
        Point2D {x=2, y=1},
        Point2D {x=4, y=4}
    ] @?= [
        Point2D {x=0, y=0},
        Point2D {x=1, y=1},
        Point2D {x=2, y=1},
        Point2D {x=3, y=1},
        Point2D {x=4, y=4},
        Point2D {x=5, y=4}
    ]

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
        ],
    testGroup "Sort List of Points" [
        testCase "Sort Points works for all points with different y coordinates"
            test_SortPoints,
        testCase "Sort Points works for points with coincident y coordinates"
            test_SortPointsCoincident
        ]
    ]
