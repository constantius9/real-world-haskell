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

newtype Vector2D = Vector2D Point2D
    deriving (Show, Eq, Ord)

vectorBy2Points :: Point2D -> Point2D -> Vector2D
vectorBy2Points a b =
    let dx = x b - x a
        dy = y b - y a
    in  Vector2D $ Point2D {x=dx, y=dy}

vectorX (Vector2D (Point2D {x=x, y=_})) = x
vectorY (Vector2D (Point2D {x=_, y=y})) = y

dotProduct2D :: Vector2D -> Vector2D -> Integer
dotProduct2D a b =
    vectorX a * vectorX b + vectorY a * vectorY b

euclideanNorm2D :: Vector2D -> Double
euclideanNorm2D v =
    sqrt . fromIntegral $ (vectorX v) ^ 2 + (vectorY v) ^ 2

angleBy3Points2D :: Point2D -> Point2D -> Point2D -> Double
angleBy3Points2D a b c =
    let ba = vectorBy2Points b a
        bc = vectorBy2Points b c
        dp = dotProduct2D ba bc
        n1 = euclideanNorm2D ba
        n2 = euclideanNorm2D bc
    in  acos( (fromIntegral dp) / (n1 * n2) )

angleWithXBy2Points2D :: Point2D -> Point2D -> Double
angleWithXBy2Points2D p@(Point2D {x=x1, y=y1}) a =
    let b = Point2D {x=x1+1, y=y1}
    in  angleBy3Points2D a p b

angleWithXByPoint2DList :: Point2D -> [Point2D] -> [Double]
angleWithXByPoint2DList p (a:[]) =
    [angleWithXBy2Points2D p a]
angleWithXByPoint2DList p (a:l) =
    [angleWithXBy2Points2D p a] ++ angleWithXByPoint2DList p l

sortedPointsByAngleWithPX :: Point2D -> [Point2D] -> [(Point2D,Double)]
sortedPointsByAngleWithPX p l =
    sortBy (\ (_,b1) (_,b2) -> (b1 :: Double) `compare` (b2 :: Double))
           (zip l (angleWithXByPoint2DList p l))

grahamScanInternal :: [Point2D] -> [Point2D] -> [Point2D]
grahamScanInternal acc [] = acc
grahamScanInternal acc l  =
    let b  = last acc
        a  = last (init acc)
        c  = head l
    in  if (direction a b c) /= Right
        then grahamScanInternal (acc ++ [c]) (tail l)
        else grahamScanInternal (init acc ++ [c]) (tail l)

grahamScan :: [Point2D] -> [Point2D]
grahamScan l =
    let sp   = sortPoints l
        p    = head sp
        spa  = sortedPointsByAngleWithPX p l
        bp   = head spa
        tspa = tail spa
        cp   = head tspa
        tsp' = [i | (i,j) <- tspa]
        b    = fst bp
        c    = fst cp
        li   = [p, b]
    in  grahamScanInternal li tsp'


class FPEq a where
    (=~) :: a -> a -> Bool

instance FPEq Double where
    x =~ y = abs ( x - y ) < (1.0e-8 :: Double)

(@?=~) :: (Show a, FPEq a) => a -> a -> Test.HUnit.Assertion
(@?=~) actual expected = actual =~ expected @? assertionMsg
    where
      assertionMsg = "Expected : "   ++ show expected ++
                     "\nActual   : " ++ show actual


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

test_VectorBy2Points1 =
    vectorBy2Points Point2D {x=0,y=1} Point2D {x=1,y=0}
    @?= Vector2D (Point2D {x=1,y=(-1)})

test_VectorBy2Points2 =
    vectorBy2Points Point2D {x=2,y=3} Point2D {x=4,y=5}
    @?= Vector2D (Point2D {x=2,y=2})

test_DotProduct2D1 =
    dotProduct2D (Vector2D (Point2D {x=1,y=(-1)})) (Vector2D (Point2D {x=2,y=2}))
    @?= 0

test_DotProduct2D2 =
    dotProduct2D (Vector2D (Point2D {x=3,y=(-2)})) (Vector2D (Point2D {x=4,y=1}))
    @?= 10

test_RightAngleBy3Points =
    angleBy3Points2D Point2D {x=0,y=1} Point2D {x=0,y=0} Point2D {x=1,y=0}
    @?=~ (pi / 2)

test_AngleBy3Points1 =
    angleBy3Points2D Point2D {x=1,y=1} Point2D {x=0,y=0} Point2D {x=1,y=0}
    @?=~ (pi / 4)

test_GrahamScan1 =
    grahamScan [
        Point2D {x=0   , y=0} -- P
      , Point2D {x=5   , y=2} -- A
      , Point2D {x=4   , y=4} -- B
      , Point2D {x=1   , y=2} -- C
      , Point2D {x=(-1), y=3} -- D
    ] @?= [
        Point2D {x=0   , y=0} -- P
      , Point2D {x=5   , y=2} -- A
      , Point2D {x=4   , y=4} -- B
      , Point2D {x=(-1), y=3} -- D
      , Point2D {x=0   , y=0} -- P
    ]

main = defaultMain tests

tests = [
    testGroup "Direction" [
        testCase "Direction for left turn"
            test_Left,
        testCase "Direction for straight line"
            test_Straight,
        testCase "Direction for right turn"
            test_Right
        ],
    testGroup "Direction List" [
        testCase "Direction List for list of 3"
            test_List3,
        testCase "Direction List for list of 5"
            test_List5,
        testCase "Direction List for list of 6"
            test_List6
        ],
    testGroup "Sort List of Points" [
        testCase "Sort Points works for all points with different y coordinates"
            test_SortPoints,
        testCase "Sort Points works for points with coincident y coordinates"
            test_SortPointsCoincident
        ],
    testGroup "Vector By 2 Points" [
        testCase "Vector By 2 Points case 1"
            test_VectorBy2Points1,
        testCase "Vector By 2 Points case 2"
            test_VectorBy2Points2
        ],
    testGroup "Dot Product 2D" [
        testCase "Dot Product 2D case 1"
            test_DotProduct2D1,
        testCase "Dot Product 2D case 2"
            test_DotProduct2D2
        ],
    testGroup "Calculate Angle by 3 points" [
        testCase "Angle calculation for right angle"
            test_RightAngleBy3Points,
        testCase "Angle calculation for Pi/4 angle"
            test_AngleBy3Points1
        ],
    testGroup "Graham Scan" [
        testCase "Graham scan for simple hull of 5 points"
            test_GrahamScan1
        ]
    ]
