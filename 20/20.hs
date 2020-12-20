import qualified Data.Map as Map
import Data.List (sort, elemIndex)
import Data.Maybe (mapMaybe, isJust, fromJust, maybe)
import Data.Either (rights)
import Data.Map ((!))

getInput :: IO [String]
getInput = do
  lines <$> readFile "input.txt"

type Tile = (Int, [String])
type TileMap = Map.Map Int Tile
type EdgeMap = Map.Map String [Int]

splitToTiles :: [String] -> [Tile]
splitToTiles []    = []
splitToTiles input =
  let
    tile = takeWhile (/="") input
    rest = splitToTiles $ drop (length tile + 1) input
    id   = read $ takeWhile (/=':') $ drop 5 $ tile !! 0 :: Int
    cont = drop 1 tile
  in
    (id, cont) : rest

getJustEdges :: [String] -> [String]
getJustEdges content =
  let
    edges = [ head content
            , last content
            , map head content
            , map last content ]
  in
    edges ++ map reverse edges

getEdges :: Tile -> [(String, [Int])]
getEdges (id, content) =
  let
    edges = getJustEdges content
  in
    map (\e -> (e, [id])) edges

findVerticalCol :: TileMap -> EdgeMap -> [Tile] -> [Tile]
findVerticalCol tileMap edgeMap prevTiles
  | length prevTiles == 12 = prevTiles
  | otherwise              =
      let
        lastId      = fst $ last prevTiles
        lastEdge    = last $ snd $ last prevTiles
        nextTileId  = head $ filter (/=lastId) $ edgeMap ! lastEdge
        nextTile    = tileMap ! nextTileId
        nextTileRot = rotateToMatchTopEdge nextTile lastEdge
      in
        findVerticalCol tileMap edgeMap (prevTiles ++ [nextTileRot])
    
rotateToMatchTopEdge :: Tile -> String -> Tile
rotateToMatchTopEdge tile@(id, content) edge
  | edgeVariant == 0 = tile
  | edgeVariant == 1 = (id, reverse content)
  | edgeVariant == 2 = (id, swapRowCol content)
  | edgeVariant == 3 = (id, reverse $ swapRowCol content)
  | otherwise        = (id, map reverse $ snd $ rotateToMatchTopEdge (id, content) (reverse edge))
  where
    edgeVariant = fromJust $ elemIndex edge $ getJustEdges $ content

swapRowCol :: [[a]] -> [[a]]
swapRowCol content = map (\i -> map (!! i) content) [0..(length content - 1)]

findHorizontalRow :: TileMap -> EdgeMap -> Tile -> [Tile]
findHorizontalRow tileMap edgeMap (id, content) = --[firstTile]
  map (\(id, content) -> (id, swapRowCol content)) $ findVerticalCol tileMap edgeMap [(id, swapRowCol content)]

removeEnds :: [a] -> [a]
removeEnds list = take (length list - 2) (drop 1 list)

removeEdges :: [String] -> [String]
removeEdges = (map removeEnds) . removeEnds


checkLine :: String -> String -> Int -> Bool
checkLine arr target index =
  let
    curr = take (length target) (drop index arr)
  in
    all (\(curr,target) -> curr == '#' || target == ' ') $ zip curr target

checkForMonster :: [String] -> (Int, Int) -> Bool
checkForMonster pic (mX,mY) =
    if (mY + (length monster) >= length pic) || (mX + (length $ monster !! 1) >= length pic)
      then False
      else
        and
          [ 
            checkLine (pic !! (mY + dY)) (monster !! dY) mX
            | dY <- [0..(length monster - 1)]
          ]
  where
    size = length pic
    monster = [ "                  # "
              , "#    ##    ##    ###"
              , " #  #  #  #  #  #   " ]

countMonsters :: [String] -> Int
countMonsters pic =
  let
    range = [0..(length pic - 1)]
  in
    length $ filter (checkForMonster pic) $ [(x,y) | x <- range, y <- range]


main :: IO ()
main = do
  input <- getInput
  let tileList = splitToTiles input
  let tiles = Map.fromList $ map (\(id, content) -> (id, (id, content))) tileList
  let edges = Map.fromListWith (++) $ concat $ map getEdges tileList

  -- A
  let singleEdges = concat $ filter ((/= 2) . length) $ Map.elems edges
  let corners = take 4 $ map fst $ filter ((== 4) . snd) $ map (\id -> (id, length $ filter (==id) singleEdges)) singleEdges

  print $ product $ corners
  
  -- B
  let startCorner = corners !! 1

  let firstCol = findVerticalCol tiles edges [tiles ! startCorner]

  let wholePicture = map (findHorizontalRow tiles edges) firstCol

  let combined = concat
               $ map (foldl1 (zipWith (++)))
               $ map (map (removeEdges . snd))
               $ wholePicture
  
  let maxMonsters = maximum $ map (countMonsters . snd) $ map (rotateToMatchTopEdge (0, combined)) $ getJustEdges combined
  let total = length $ filter (=='#') $ concat combined
  print $ total - maxMonsters * 15
