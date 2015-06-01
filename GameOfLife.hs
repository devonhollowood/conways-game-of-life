module GameOfLife (
    Grid (nrows, ncols),
    CellState (..),
    Cell (cellRow, cellCol, cellState),
    makeGrid,
    getCell,
    cellList,
    updateGrid
    ) where
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

data Grid = Grid {
    nrows :: Int,
    ncols :: Int,
    cells :: Vector Cell
} deriving Show

data Cell = Cell {
    cellRow   :: Int,
    cellCol   :: Int,
    cellState :: CellState
} deriving Show

data CellState = Live | Dead deriving (Eq, Show)

makeGrid :: Int -> Int -> [(Int, Int)] -> Grid
makeGrid nrows ncols livecells
    | nrows < 1 || ncols < 1 = error "Grid must have > 0 rows and > 0 cols"
    | otherwise = Grid nrows ncols $
          V.fromList[Cell r c (state r c) | r <- [0..nrows-1],
                                              c <- [0..ncols-1]]
    where state r c = if (r,c) `elem` livecells
                      then Live
                      else Dead

getCell :: Grid -> Int -> Int -> Cell
getCell grid row col =
    let r = row `mod` nrows grid
        c = col `mod` ncols grid
    in cells grid ! (r * ncols grid + c)

cellList :: Grid -> [Cell]
cellList = V.toList . cells

neighbors :: Grid -> Cell -> [Cell]
neighbors grid cell = let (row, col) = (cellRow cell, cellCol cell) in
    fmap (uncurry $ getCell grid)
        [(row-1, col-1), (row-1, col), (row-1, col+1),
         (row  , col-1),               (row  , col+1),
         (row+1, col-1), (row+1, col), (row+1, col+1)]

updateCell :: Grid -> Cell -> Cell
updateCell grid cell
    | is_live cell && live_neighbors < 2 = Cell r c Dead -- Live and underpop
    | is_live cell && live_neighbors < 4 = Cell r c Live -- Live and stable
    | is_live cell                       = Cell r c Dead -- Live and overpop
    | live_neighbors == 3                = Cell r c Live -- Dead but reproduced
    | otherwise                          = Cell r c Dead -- Dead and stays dead
    where live_neighbors = length . filter (is_live) $ neighbors grid cell
          is_live cell = cellState cell == Live
          (r, c) = (cellRow cell, cellCol cell)

updateGrid :: Grid -> Grid
updateGrid grid = let (r, c) = (nrows grid, ncols grid) in
    Grid r c $ fmap (updateCell grid) (cells grid)
