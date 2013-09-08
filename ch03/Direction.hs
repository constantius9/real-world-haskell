data Direction = Left
               | Right
               | Straight

data Point2D = Point2D { x :: Double
                       , y :: Double
                       } deriving (Show, Eq, Ord)