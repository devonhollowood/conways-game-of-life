import System.Environment (getArgs)
import Graphics.Gloss
import GameOfLife
import Graphics.Gloss.Interface.Pure.Simulate
import Text.Parsec
import Text.Parsec.String (Parser)

cellWidth  = 10 :: Int
cellHeight = 10 :: Int

main = do
    [speed,filename] <- getArgs
    grid <- parseFile filename
    let xpix = cellWidth * ncols grid
        ypix = cellHeight * nrows grid
    simulate
           (InWindow "Conway's Game of Life" (xpix, ypix) (10, 10))
           white
           (read speed)
           grid
           (gridPic xpix ypix)
           iter

parseFile :: String -> IO Grid
parseFile file = do
    contents <- readFile file
    case parse pFile "" contents of
        Right g -> return g
        Left e  -> error (show e)

pFile :: Parser Grid
pFile = do
    many space
    width <- pInt
    many space
    height <- pInt
    many space
    liveCells <- pCell `sepEndBy` many1 space
    eof
    return $ makeGrid width height liveCells

pInt :: Parser Int
pInt = many1 digit >>= return . read

pCell :: Parser (Int, Int)
pCell = do
    char '('
    x <- pInt
    char ','
    optional $ char ' '
    y <- pInt
    char ')'
    return (x,y)

gridPic :: Int -> Int -> Grid -> Picture
gridPic xpix ypix grid = pictures $ fmap (makeCell xpix ypix) (cellList grid)

makeCell xpix ypix cell = uncurry translate (position xpix ypix cell) $
                              if cellState cell == Live
                              then liveCell
                              else deadCell

position :: Int -> Int -> Cell -> (Float, Float)
position xpix ypix cell =
    ((fromIntegral (cellWidth * cellCol cell) - xoffset),
     negate (fromIntegral (cellHeight * cellRow cell) - yoffset))
    where xoffset = fromIntegral (xpix-cellWidth) / 2.0
          yoffset = fromIntegral (ypix-cellWidth) / 2.0

liveCell :: Picture
liveCell = color blue $
    rectangleSolid (fromIntegral cellWidth) (fromIntegral cellHeight)

deadCell :: Picture
deadCell = color (black) $
    rectangleWire (fromIntegral cellWidth) (fromIntegral cellHeight)

iter :: ViewPort -> Float -> Grid -> Grid
iter _ _ grid = updateGrid grid
