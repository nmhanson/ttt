import Data.List
import Text.Read

data Cell n = X | O | Empty n deriving Show
data State w = Winner w | Cats | Ongoing

type Piece = Cell Int
type Board = [Piece]
type GameState = State Piece
type Vector = (Int, Int, Int)
type Move = Maybe Int

-- List of winning triples on the board
winVecs :: [Vector]
winVecs = [ (0, 1, 2) -- Rows
          , (3, 4, 5)
          , (6, 7, 8)
----------------------------------
          , (0, 3, 6) -- Columns
          , (1, 4, 7)
          , (2, 5, 8)
----------------------------------
          , (0, 4, 8) -- Diagonals
          , (6, 4, 2)
          ]

-- Horizontal separator for readability
hSep :: String
hSep = "+---+---+---+\n"

-- Initialize the game
main :: IO ()
main = play initBd X

-- Game loop
play :: Board -> Piece -> IO ()
play b p = do
    putStrLn $ "It is your turn, " ++ show p ++ "!"
    putStrLn $ boardStr b
    input <- getLine
    let m  = parseMove input b
        nb = (applyMv b m p)
    case getState nb of (Winner player) -> printWinner player
                        Cats            -> declareCats
                        Ongoing         -> play nb (np m) 
    where np Nothing     = p
          np (Just _)    = nextPiece p
          printWinner pl = putStrLn $ show pl ++ " wins!"
          declareCats    = putStrLn "It's a tie!"

-- Scan the board for winning triples
getState :: Board -> GameState
getState bd = checkCats bd $ find winner (map (checkVec bd) winVecs)
              where winner (Winner _) = True
                    winner _          = False
                    newState (Just w) = w
                    newState Nothing  = Ongoing

-- If there isn't a winner yet, check to see if board has been filled
checkCats :: Board -> Maybe GameState -> GameState
checkCats _ (Just w) = w
checkCats bd Nothing = if all filled bd then Cats else Ongoing
                       where filled (Empty _) = False
                             filled _         = True

-- Check a triplet on the board to determine whether a player has won
checkVec :: Board -> Vector -> GameState
checkVec bd (a, b, c) = newState (bd !! a, bd !! b, bd !! c)
                        where newState (X, X, X) = Winner X
                              newState (O, O, O) = Winner O
                              newState _         = Ongoing

-- Creates a board from the previous board with the move applied
applyMv :: Board -> Move -> Piece -> Board
applyMv b (Just n) p = (take (n - 1) b) ++ p : (drop n b)
applyMv b Nothing p = b

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

-- Validate a move
parseMove :: String -> Board -> Move
parseMove s b = readMaybe s >>= verifyMvValid >>= verifyMvNoCollision b

-- Verify a move falls within the boundries of the board
verifyMvValid :: Int -> Move
verifyMvValid mvNum
    | mvNum > 9 = Nothing
    | mvNum < 1 = Nothing
    | otherwise = Just $ mvNum

-- Verify a move doesn't collide with a piece that has already been placed
verifyMvNoCollision :: Board -> Int -> Move
verifyMvNoCollision bd cellNum 
    | collides (bd !! (cellNum - 1)) = Nothing
    | otherwise = Just cellNum
    where collides (Empty _) = False
          collides _         = True


-- Toggle which piece is about to be played
nextPiece :: Piece -> Piece
nextPiece X = O
nextPiece O = X
