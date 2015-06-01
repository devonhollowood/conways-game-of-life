import System.Environment (getArgs)
import Graphics.Gloss
import GameOfLife
import Graphics.Gloss.Interface.Pure.Simulate

cellWidth  = 30 :: Int
cellHeight = 30 :: Int

main = do
    [filename] <- getArgs
    grid <- parseFile filename
    let xpix = cellWidth * nrows grid
        ypix = cellHeight * ncols grid
    simulate
           (InWindow "Conway's Game of Life" (xpix, ypix) (10, 10))
           white
           1
           grid
           gridPic
           iter

parseFile :: String -> IO Grid
parseFile _ = return $ makeGrid 6 6 [(2,3), (2,4), (2,5), (3,2), (3,3), (3,4)]

gridPic :: Grid -> Picture
gridPic grid = pictures (fmap make_cell $ cellList grid)
        where make_cell c = color (if cellState c == Live then red else white)
                            $ translate
                                (fromIntegral $ cellWidth * cellRow c)
                                (fromIntegral $ cellHeight*cellCol c)
                            $ rectangleSolid
                                (fromIntegral cellWidth)
                                (fromIntegral cellHeight)

iter :: ViewPort -> Float -> Grid -> Grid
iter _ _ grid = updateGrid grid
