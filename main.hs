import Data.List

data Move n = Invalid | Valid n deriving Show
data Cell n = X | O | Empty n deriving Show
type Board = [Cell Int]

-- Horizontal separator for readability
hSep = "+---+---+---+\n"

-- Initialize the game
main :: IO ()
main = play initBd

-- Game loop
play :: Board -> IO ()
play b = do
    putStrLn $ boardStr b
    input <- getLine
    let m = parseMove input
    play (applyMv b m)

-- Creates a board from the previous board with the move applied
applyMv :: Board -> Move Int -> Board
applyMv b (Valid n) = (take (n - 1) b) ++ X : (drop n b)
applyMv b Invalid = b

-- Create board of empty cells
initBd :: Board
initBd = map Empty [1..9]

-- Get a string representation of the game board
boardStr :: Board -> String
boardStr b = hSep ++ (intercalate hSep $ boardStr' b) ++ hSep

-- Helper for boardStr
boardStr' :: Board -> [String]
boardStr' [] = []
boardStr' b = ("| " ++ (intercalate " | " rowText) ++ " |\n") : rest
                where row = take 3 b
                      rest = boardStr' (drop 3 b)
                      rowText = map cellStr row

-- Determine what to show per cell
cellStr :: Cell Int -> String
cellStr (Empty n) = show n
cellStr c = show c

-- Validate whether a move is valid and parse it to an Int
parseMove :: String -> Move Int
parseMove s
    | moveNum > 9 = Invalid
    | moveNum < 1 = Invalid
    | otherwise   = Valid moveNum
    where moveNum = read s

