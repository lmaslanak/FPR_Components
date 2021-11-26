import Data.List ( groupBy )

-- Point x y z
data Point = Point Int Int Int deriving (Eq, Show)

data Cube = Cube {start :: Point, size :: Int} deriving (Eq, Show)

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

overlap :: Cube -> Cube -> Bool
overlap (Cube (Point x00 y00 z00) size0) (Cube (Point x10 y10 z10) size1) = vys
  where
    x01 = x00 + (size0 - 1)
    y01 = y00 + (size0 - 1)
    z01 = z00 + (size0 - 1)

    x11 = x10 + (size1 - 1)
    y11 = y10 + (size1 - 1)
    z11 = z10 + (size1 - 1)

    vys = (x00 <= x11 && x01 >= x10) && (y00 <= y11 && y01 >= y10) && (z00 <= z11 && z01 >= z10)

printIt :: [[Cube]] -> IO ()
printIt components = putStr (concat [show component ++ "\n" | component <- components])

mostRight :: [Cube] -> [Cube]
mostRight [x] = take 1 [x]
mostRight cubes =take 1 [Cube (Point x0 y0 z0) size0 | (Cube (Point x0 y0 z0) size0) <- cubes, y0 + size0 == maxIndexY] where
            maxIndexY = maximum [ y + size| Cube (Point _ y _) size <- cubes]


breakIt :: [Cube] -> [(Cube, Int)]
breakIt list =
  let count = 0
      indexedList = [(x, 0) | x <- list]
   in tmp indexedList count
  where
    tmp :: [(Cube, Int)] -> Int -> [(Cube,Int)]
    tmp [x] count = let (cube, value) = x in [(cube,value+count)]
    tmp (x : y : rest) count = let (cube, value) = x in if overlap (fst x) (fst y) then (cube, value+count): tmp (y:rest) count else (cube, value+count): tmp (y:rest) (count+1)

groupIt:: [(Cube, Int)] -> [[(Cube, Int)]]
groupIt list = groupBy (\x y -> snd x == snd y) list

components:: [Cube] -> [[Cube]]
components list = let grouped = groupIt $ breakIt list in [[a | (a,_) <- grouped !! x] | x <-[0..((length grouped)- 1)]]

--protnutno :: Cube -> Cube -> [Cube]
--protnuto Cube1 Cube2 = if intersect [Cube1] [Cube2] == 1 then [[Cube2]]
{-
components :: [Cube] -> [[Cube]] 
components (x:sampleInput) =  adding sampleInput [[x]] [] --funkce na rozdeleni 

-- connect :: [[Cube]] -> [[Cube]]
-- connect [x] = [x]
-- connect ((cube1:cubes1):(cube2:cubes2):others) = if elem cube2 (cube1:cubes1) == True && cube1 == cube2  then connect ((cube1:cubes2):(others)) else connect ((cube2:cubes2):(others))
--fce [[cube]] cube -> b -> true -> [ [cube]++cube]
adding :: [Cube] -> [[Cube]] -> [Cube] -> [[Cube]]
adding [] _ _ = []
adding (cube:cubes) [] _ = [cube] : adding cubes [[cube]] []
adding (cube:cubes) [x:xs] y 
                            | intersect cube x == True = adding cubes [(x:xs)++y++[cube]] [] 
                            | otherwise = adding (cube:cubes) [xs] (y++[x])

--fce která když intersect true tak přidá cube do [[cubes]]

intersect :: Cube -> Cube -> Bool
intersect (Cube (Point x00 y00 z00) size0) (Cube (Point x10 y10 z10) size1) = vys
  where
    x01 = x00 + (size0 - 1)
    y01 = y00 + (size0 - 1)
    z01 = z00 + (size0 - 1)

    x11 = x10 + (size1 - 1)
    y11 = y10 + (size1 - 1)
    z11 = z10 + (size1 - 1)

    vys = (x00 <= x11 && x01 >= x10) && (y00 <= y11 && y01 >= y10) && (z00 <= z11 && z01 >= z10)

-}
