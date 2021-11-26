# FPR_Components

6 - Components
Lets assume, that we have a 3D model composed from cubes in standard Euclidean space. In our model, we are using only positive integers. Each of these cubes is defined by its corner with the smallest x, y and z coordinates and by its size. It will be represented by following type Cube

-- Point x y z
data Point = Point Int Int Int deriving (Eq, Show)
data Cube = Cube {start::Point, size::Int }  deriving (Eq, Show)
Cubes in our model do overlap. To overlap, they need to have some shared volume, just touching is not enough. A component is a set of cubes that are overlapping. In other words, if we want to add a cube x to a component, we need to find a cube from the component, that overlaps with the cube x

Write a function components that takes a list of cubes ([Cube]) and as a result divides these cubes into components. The result will be [[Cube]], where each of inner lists represents a component (as was defined above). You can use function printIt to print each of these components on a separated line.

sampleInput :: [Cube]
sampleInput = [Cube { start = Point 0 0 0, size = 5},
               Cube { start = Point 4 4 4, size = 5},
               Cube { start = Point 8 8 8, size = 4}, 
               Cube { start = Point 12 12 12, size = 2},
               Cube { start = Point 13 13 13, size = 2},
               Cube { start = Point 10 10 0, size = 2},
               Cube { start = Point 9  9 0, size = 4}]

printIt :: [[Cube]] -> IO ()
printIt components = putStr (concat [show component ++ "\n" |component<-components])

components::[Cube] -> [[Cube]]
*Main> printIt (components sampleInput) 
[Cube {start = Point 0 0 0, size = 5},Cube {start = Point 4 4 4, size = 5},Cube {start = Point 8 8 8, size = 4}]
[Cube {start = Point 12 12 12, size = 2},Cube {start = Point 13 13 13, size = 2}]
[Cube {start = Point 10 10 0, size = 2},Cube {start = Point 9 9 0, size = 4}]
