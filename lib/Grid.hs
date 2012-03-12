module Grid where

data Grid a = Grid { width :: Int
                   , height :: Int
                   , vals :: [[a]]
                   } deriving (Show, Eq)

data Direction = Rgt | Dwn | DwnRgt | DwnLft
  deriving (Show, Eq)

data Coord = Coord { x :: Int
                   , y :: Int
                   } deriving (Show, Eq)

fromLists :: [[a]] -> Grid a
fromLists xs =
  Grid w h $ map (take w . cycle) xs
    where
      w = maximum $ map length xs
      h = length xs


directionVector :: Direction -> Coord
directionVector Rgt    = Coord 1 0
directionVector Dwn    = Coord 0 1
directionVector DwnRgt = Coord 1 1
directionVector DwnLft = Coord (-1) 1

path :: Coord -> Direction -> Int -> [Coord]
path start dir len = map mkCoord $ take len [0..]
  where
    mkCoord = cAdd start . cMult dirV
    dirV = directionVector dir

cAdd :: Coord -> Coord -> Coord
cAdd a b = Coord (x a + x b) (y a + y b)

cMult :: Coord -> Int -> Coord
cMult c i = Coord newX newY
  where
    newX = i * x c
    newY = i * y c

grabCoord :: Grid a -> Coord -> a
grabCoord grid coord = (vals grid) !! (y coord) !! (x coord)

directions :: [Direction]
directions = [Rgt, DwnRgt, Dwn, DwnLft]

pathsForCoord :: Coord -> Int -> [[Coord]]
pathsForCoord c l = map (\d -> path c d l) directions

pathsForGridCoord :: Grid a -> Coord -> Int -> [[Coord]]
pathsForGridCoord grid startCoord l = map (\d -> path startCoord d l) $ filter directionGood directions
  where
    directionGood dir = xGood && yGood
      where
        xGood = eX >= 0 && eX < width grid
        yGood = eY >= 0 && eY < height grid
        dirV = directionVector dir
        endCoord = cAdd startCoord $ cMult dirV $ l - 1
        eX = x endCoord
        eY = y endCoord


coordsForRange :: Int -> Int -> [Coord]
coordsForRange x y = concatMap mkCoords [0..y]
  where
    mkCoords j = map mkCoord [0..x]
      where
        mkCoord i = Coord i j

allPathsForGrid :: Grid a -> Int -> [[Coord]]
allPathsForGrid g l = concatMap (\c -> pathsForGridCoord g c l) $ coordsForRange dx dy
  where
    dx = (width g)  - 1
    dy = (height g) - 1

pathToVals :: Grid a -> [Coord] -> [a]
pathToVals g cs = map (grabCoord g) cs
