import Data.List


sampleInput :: [Cube]
sampleInput =
  [ Cube {start = Point 0 0 0, size = 5},
    Cube {start = Point 4 4 4, size = 5},
    Cube {start = Point 8 8 8, size = 4},
    Cube {start = Point 12 12 12, size = 2},
    Cube {start = Point 13 13 13, size = 2},
    Cube {start = Point 10 10 0, size = 2},
    Cube {start = Point 9 9 0, size = 4}
  ]

-- Point x y z
data Point = Point Int Int Int deriving (Eq, Show)
data Cube = Cube {start :: Point, size :: Int} deriving (Eq, Show)


overLap :: Cube -> Cube -> Bool
overLap (Cube (Point x00 y00 z00) size0) (Cube (Point x01 y01 z01) size1) = res
  where
    x10 = x00 + size0
    x11 = x01 + size1

    y10 = y00 + size0
    y11 = y01 + size1

    z10 = z00 + size0
    z11 = z01 + size1  

    res = (x10 > x01) && (x11 > x00) && (y10 > y01) && (y11 > y00) && (z10 > z01) && (z11 > z00)

components :: [Cube] -> [[Cube]]
components [] = []
components (x:xs) = componentsList x xs 0 : components ( (x:xs) \\ (componentsList x xs 0))
    where
    componentsList :: Cube -> [Cube] -> Int -> [Cube]
    componentsList _ [] _ = []
    componentsList a (b:bs) n | overLap a b == True && n == 0 = [a] ++ [b] ++ componentsList b bs 1
                              | overLap a b == True && n == 1 = [b] ++ componentsList b bs 1
                              | overLap a b == False = componentsList a bs n


printIt :: [[Cube]] -> IO()
printIt components = putStr (concat [show component ++ "\n" | component <- components])
