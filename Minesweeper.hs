

import System.Random

data Cell = Mine | Safe Int | Hidden deriving (Show, Eq)
type Board = [[Cell]]

generateMines :: Int -> Int -> Int -> IO [(Int, Int)]
generateMines _ 0 _ = return []
generateMines width remainingMines seed = do
    gen <- newStdGen
    let (row, col) = (randomR (0, width - 1) gen, randomR (0, width - 1) gen)
    rest <- generateMines width (remainingMines - 1) (snd row)
    return $ (fst row, fst col) : rest

initializeBoard :: Int -> Int -> Int -> Board
initializeBoard width height numMines =
    let mines = take numMines $ zip (randoms (mkStdGen numMines)) (randoms (mkStdGen (numMines + 1)))
        board = [[if (row, col) `elem` mines then Mine else Safe 0 | col <- [0..width - 1]] | row <- [0..height - 1]]
    in updateNeighbours board

updateNeighbours :: Board -> Board
updateNeighbours board = map (\(r, row) -> map (\(c, cell) -> updateCellNeighbour board r c cell) (zip [0..] row)) (zip [0..] board)

updateCellNeighbour :: Board -> Int -> Int -> Cell -> Cell
updateCellNeighbour board r c cell =
    case cell of
        Mine -> Mine
        Safe _ -> Safe $ countMines board r c
        Hidden -> Hidden

countMines :: Board -> Int -> Int -> Int
countMines board r c =
    let neighbours = [(r + i, c + j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]
        validNeighbours = filter (\(x, y) -> x >= 0 && x < length board && y >= 0 && y < length (head board)) neighbours
    in length $ filter (\(x, y) -> isMine (board !! x !! y)) validNeighbours

isMine :: Cell -> Bool
isMine Mine = True
isMine _ = False

data GameState = Ongoing | Won | Lost deriving (Show, Eq)

revealCell :: Board -> Int -> Int -> Board
revealCell board row col =
    case board !! row !! col of
        Hidden -> if isMine (board !! row !! col) then revealMines board else revealSafeCells board row col
        _ -> board

revealSafeCells :: Board -> Int -> Int -> Board
revealSafeCells board row col =
    let updatedBoard = updateCell board row col
    in if countHiddenCells updatedBoard == 0
            then foldl (\b (r, c) -> revealSafeCells b r c) updatedBoard (getNeighbours (length board) (length (head board)) row col)
            else updatedBoard

revealMines :: Board -> Board
revealMines board = map (\row -> map (\cell -> if isMine cell then cell else Safe 0) row) board

updateCell :: Board -> Int -> Int -> Board
updateCell board row col =
    let value = countMines board row col
    in updateCell' board row col value

updateCell' :: Board -> Int -> Int -> Int -> Board
updateCell' board row col value =
    let (before, cell:after) = splitAt row board
        (left, _:right) = splitAt col cell
        updatedCell = Safe value
        newRow = left ++ updatedCell : right
    in before ++ (newRow : after)

countHiddenCells :: Board -> Int
countHiddenCells board = sum $ map (length . filter (== Hidden)) board

getNeighbours :: Int -> Int -> Int -> Int -> [(Int, Int)]
getNeighbours height width row col =
    let neighbours = [(row + i, col + j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]
    in filter (\(x, y) -> x >= 0 && x < height && y >= 0 && y < width) neighbours

checkGameState :: Board -> GameState
checkGameState board
    | any (\row -> any (\cell -> case cell of {Mine -> True; _ -> False}) row) board = Lost
    | all (\row -> all (\cell -> case cell of {Safe _ -> True; _ -> False}) row) board = Won
    | otherwise = Ongoing

main :: IO ()
main = do
    putStrLn "Welcome to Minesweeper!"
    let width = 10
        height = 10
        numMines = 10
    mines <- generateMines width numMines numMines
    let board = initializeBoard width height numMines
    playGame board numMines

playGame :: Board -> Int -> IO ()
playGame board remainingMines = do
    printBoard board
    putStrLn "Enter row and column to reveal (e.g., '1 2'): "
    input <- getLine
    let [row, col] = map (\x -> read x :: Int) (words input)
        newBoard = revealCell board row col
        gameState = checkGameState newBoard
    case gameState of
        Won -> putStrLn "Congratulations! You won!"
        Lost -> do
            putStrLn "Game over! You lost!"
            printBoard $ revealMines newBoard
        Ongoing -> playGame newBoard remainingMines

printBoard :: Board -> IO ()
printBoard board = mapM_ putStrLn $ map (concatMap showCell) board

showCell :: Cell -> String
showCell cell =
    case cell of
        Mine -> "* "
        Safe n -> show n ++ " "
        Hidden -> "_ "

