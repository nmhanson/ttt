import Data.List
import Text.Read

data Move n = Invalid | Valid n deriving Show
data Cell n = X | O | Empty n deriving Show
type Piece = Cell Int
type Board = [Piece]

-- Horizontal separator for readability
hSep :: String
hSep = "+---+---+---+\n"

-- Initialize the game
main :: IO ()
main = play initBd X

-- Game loop
play :: Board -> Piece -> IO ()
play b p = do
    putStrLn $ boardStr b
    input <- getLine
    let m = parseMove input
    play (applyMv b m p) (np m)
    where np Invalid   = p
          np (Valid _) = nextPiece p

-- Creates a board from the previous board with the move applied
applyMv :: Board -> Move Int -> Piece -> Board
applyMv b (Valid n) p = (take (n - 1) b) ++ p : (drop n b)
applyMv b Invalid p = b

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
cellStr :: Piece -> String
cellStr (Empty n) = show n
cellStr c = show c

-- Validate whether a move is valid and parse it to an Int
parseMove :: String -> Move Int
parseMove s
    | moveNum input > 9 = Invalid
    | moveNum input < 1 = Invalid
    | otherwise   = Valid $ moveNum input
    where moveNum (Just n) = n
          moveNum Nothing = (-1) -- malformed input
          input = readMaybe s :: Maybe Int

-- Toggle which piece is about to be played
nextPiece :: Piece -> Piece
nextPiece X = O
nextPiece O = X
