import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import System.Environment
import System.IO
import Control.Monad
import Data.Monoid

-- Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []]
-- (Node 5 [Node 2 [Node 1 [Node 3 [], Node 4 []] , Node 2 [] ] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []])
-- Every Node of Rose Tree can have multiple children
data Rose a = Node a [Rose a] deriving (Show)


--Return number of nodes in tree 
-- input: size (Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []]) 
-- output: 6
size:: Rose a->Int
size (Node _ list) = 1 + sum (map (\elem->size elem) list) 

--Calculate height of tree
-- input: height (Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []]) 
-- output: 2
height:: Rose a->Int
height (Node _ []) = 0
height (Node _ list) = 1 + maximum (map (\elem->height elem) list)


--Return number of leaves of tree
-- input: leavesCount (Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []]) 
-- output: 4
leavesCount:: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ list) = sum (map (\elem->leavesCount elem) list)

--Return values of all leaves of tree
-- input: leaves (Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []]) 
-- output: [2,4,8,7]
leaves:: Rose a -> [a]
leaves (Node a []) = [a]
leaves (Node _ list) = concat( map (\elem->leaves elem) list)


--Return values of elements that are on passed depth
-- input: elemsOnDepth (Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []]) 2
-- output: [4,8]
elemsOnDepth:: Rose a -> Int -> [a]
elemsOnDepth (Node a _) 0 = [a]
elemsOnDepth (Node _ []) _ = []
elemsOnDepth (Node _ list) x = concat(map (\elem -> elemsOnDepth elem (x-1)) list)


-- input: fmap (+3) (Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []])
-- output: Node 8 [Node 5 [],Node 6 [Node 7 [],Node 11 []],Node 10 []]
instance Functor Rose where
    fmap f (Node a list) = Node (f a) (map (\elem -> fmap f elem) list)


-- input: foldRose (+) 0 ( Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []])
-- output: 29
foldRose :: (a -> b -> b) -> b -> Rose a -> b
foldRose f acc (Node a list) = foldl (\acc' elem -> foldRose f acc' elem) (f a acc) list
    

-- This is representing data types for players, the game board, moves, and field states
data Player = P1 | P2 
newtype Row a = Row [a] 
data Board a = Board { board :: [Row a], playerToMove :: Player } 
newtype Move = Move (Int, Int) 
data Field = X | O | E deriving Eq




-- a -> (b , a )    =  trenutnoStanje -> ( rezultatOperacije , novoStanje )
-- | A wrapper for stateful operations on a game state.
-- The GameStateOp type encapsulates a function that takes a current state and returns a result along with the new state.
newtype GameStateOp a b = GameStateOp { runGameStateOp :: a -> (b, a)} 


-- Functor instance for GameStateOp allows applying a function to the result of a state operation, without altering the state itself.
instance Functor (GameStateOp s) where
  fmap f (GameStateOp op) = GameStateOp (\s -> let (res, newState) = op s in (f res, newState))



-- Applicative instance for GameStateOp enables sequencing of stateful operations where the result of one operation can be used as input to another, while threading the state through.
instance Applicative (GameStateOp s) where
  pure a = GameStateOp (\s -> (a, s))
  (GameStateOp sf) <*> (GameStateOp sa) = GameStateOp (\state -> let (fn,state1) = sf state
                                                                     (a, state2) = sa state1 in (fn a, state2))


-- Monad instance for GameStateOp provides a way to chain operations on states, allowing the output and new state of one operation to feed into the next.
instance Monad (GameStateOp s) where
  return = pure
  (GameStateOp h) >>= f = GameStateOp $ \s -> let (a, newState) = h s
                                                  (GameStateOp g) = f a
                                                  in g newState


-- | An extension of GameStateOp that additionally tracks the history of states through which the game has progressed.
-- The GameStateOpHistory type encapsulates a function that, given a current state, returns a result and a list of past states.
newtype GameStateOpHistory a b = GameStateOpHistory { runGameStateOpHistory :: a -> (b, [a]) }

-- Functor instance for GameStateOpHistory allows applying a function to the result, similarly to GameStateOp, but also tracks the history of states.
instance Functor (GameStateOpHistory s) where
  fmap f (GameStateOpHistory op) = GameStateOpHistory (\states -> let (res, newStates) = op states in (f res, newStates))

-- Applicative instance for GameStateOpHistory enables sequencing of operations with state history tracking.
instance Applicative (GameStateOpHistory s) where
  pure a = GameStateOpHistory (\s -> (a, [s]))
  (GameStateOpHistory sf) <*> (GameStateOpHistory sa) = GameStateOpHistory (\state -> let (fn, state1) = sf state
                                                                                          (a, state2) = sa (head state1)
                                                                                      in (fn a, state1 ++ state2))

-- Monad instance for GameStateOpHistory provides chaining of operations with an emphasis on tracking the history of all states encountered.
instance Monad (GameStateOpHistory a) where
  return = pure
  (GameStateOpHistory h) >>= f = GameStateOpHistory (\s ->let (a, states) = h s 
                                                              GameStateOpHistory g = f a 
                                                              (b, newStates) = g (head states)
                                                           in (b, newStates ++ states))




-- show
-- input: show (Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []])
-- output: "Node 5 [Node 2 [],Node 3 [Node 4 [],Node 8 []],Node 7 []]"
instance Eq Player where
  P1 == P1 = True
  P2 == P2 =True
  _ == _ = False
instance Show Player where
    show P1 = "P1"
    show P2 = "P2"
instance Show Field where
  show X = "X"
  show O = "O"
  show E = "P"

instance Show a => Show (Row a) where
    show (Row values) = "\n "++ concatMap showValue values ++ " | "
      where
        showValue value = " | " ++ show value

instance Show a => Show (Board a) where
  show (Board rows player) = "Board { board = " ++ show rows ++ ", playerToMove = " ++ show player ++ " } \n"
instance Show Move where
  show (Move (x,y)) = "(" ++ show x ++ ", "++show y ++ ")"



-- Functions

-- input: validMoves (Board [Row [X,O,X], Row [X,X,X], Row[E,E,E] ] P2)
-- output: [(2,0),(2,1),(2,2)]
validMoves:: Board Field -> [(Int,Int)]
validMoves (Board rows _) = [(rowIndexs,colIndex) | (rowIndexs , row) <- zip [0..] rows, (colIndex, field) <- zip [0..] (rowFields row), field == E] 
            where rowFields (Row fields) = fields

-- input: boardToRows (Board [Row [X,O,X], Row [X,X,X], Row[E,E,E] ] P2)
-- output:
-- [
--  | X | O | X | ,
--  | X | X | X | ,
--  | P | P | P | ]
boardToRows :: Board Field -> [Row Field]
boardToRows = board


-- Function same as validMoves, but it also check if the game is ended
validMovesGameTree :: Board Field -> [(Int, Int)]
validMovesGameTree board
  | isGameOver board = []
  | otherwise = [(rowIndex, colIndex) | (rowIndex, row) <- zip [0..] rows, (colIndex, field) <- zip [0..] (rowFields row), field == E]
  where
    rows = boardToRows board
    rowFields (Row fields) = fields


-- This funcion return new state of board after move we passed
-- input: newStateAfterMove (Board [Row [X,O,X], Row [X,X,X], Row[E,E,E] ] P2) (Move(2,0))
-- output: Board { board = [
--  | X | O | X | ,
--  | X | X | X | ,
--  | O | P | P | ], playerToMove = P1 } 
newStateAfterMove:: Board Field -> Move -> Board Field
newStateAfterMove (Board rows player) (Move (rowIndex, colIndex)) = 
    Board (newRows rows rowIndex (updateColumn (getRow rows rowIndex) colIndex (getPlayerField player))) (togglePlayer player)
    where
        newRows rows i newRow = take i rows ++ [newRow] ++ drop (i + 1) rows
        getRow rows i = rows !! i
    
        updateColumn (Row fields) i newField = Row (take i fields ++ [newField] ++ drop (i + 1) fields)
    
        getPlayerField P1 = X
        getPlayerField P2 = O
    
        togglePlayer P1 = P2
        togglePlayer P2 = P1





--Function that test some casses if they are working properly
testIsGameOver:: [Bool]
testIsGameOver = [isGameOver (Board [Row [X,O,X], Row [X,X,X], Row[E,E,E] ] P2) , isGameOver (Board [Row [X,O,X], Row [X,E,E], Row[X,E,E] ] P2), 
        isGameOver (Board [Row [X,O,X], Row [E,X,E], Row[E,E,X] ] P2),isGameOver (Board [Row [X,O,X], Row [E,X,E], Row[X,E,E] ] P2), isGameOver (Board [Row [X,O,X], Row [O,X,O], Row[O,X,O] ] P2), 
        isGameOver (Board [Row [E,E,E], Row [E,E,E], Row[E,E,E] ] P2)]


--Function that checkes if the game is over
isGameOver :: Board Field -> Bool
isGameOver board =
  isWinningState board || isBoardFull board
  where
    isWinningState :: Board Field -> Bool
    isWinningState (Board rows _) =
      any (\(Row fields)-> allSame fields) rows ||
      any (\colIndex -> allSame (getColumn colIndex rows)) [0..2] ||
      allSame (getDiagonal1 rows) || allSame (getDiagonal2 rows)

    allSame:: [Field] -> Bool
    allSame [] = True
    allSame [_] = True
    allSame (x:y:xs) = x == y && (x /= E) && allSame(y:xs) 

    getColumn :: Int -> [Row Field] -> [Field]
    getColumn index rows = map (\(Row fields) -> fields !! index) rows
        
    getDiagonal1 :: [Row Field] -> [Field]
    getDiagonal1 rows = [ row !! rowIndex | (rowIndex, Row row) <- zip [0..] rows]
    
    getDiagonal2 :: [Row Field] -> [Field]
    getDiagonal2 rows = [row !! (2 - rowIndex) | (rowIndex, Row row) <- zip [0..] rows]
    
    allFull:: [Field] -> Bool
    allFull [] = True
    allFull [_] = True
    allFull (x:y:xs) = (x /= E) && allFull(y:xs) 

    isBoardFull :: Board Field -> Bool
    isBoardFull (Board rows _) = all (\(Row fields)-> allFull fields) rows


-- Gives all possible outcomes for given board
-- input: createGameTree  (Board [Row [X,O,X], Row [X,X,O], Row[E,E,E] ] P2)
createGameTree :: Board Field -> Rose (Board Field)
createGameTree board = Node board children
  where
    currentPlayer = playerToMove board
    moves = validMovesGameTree board
    children = map (\move -> createGameTree (newStateAfterMove board (Move move))) moves



-- Executes a move on the game board, updating its state and evaluating if the game has concluded.
-- The function takes a `Move` (with row and column indices) and returns a `GameStateOp` that, when run,
-- updates the board according to the move and returns a tuple indicating whether the game is over and the new board state.
-- It leverages the `newStateAfterMove` function to apply the move and toggle the player turn,
-- and it may also involve checking for game-ending conditions through `isGameOver`.
-- Usage:
-- applyMove (Move (x, y)) - Applies the move at the specified row and column, updating the game state accordingly.
applyMove :: Move -> GameStateOp (Board Field) Bool
applyMove (Move (x, y)) = GameStateOp $ \board ->
  let newBoard = newStateAfterMove board (Move(x,y))
  in (isGameOver newBoard, newBoard)


iksOksInitialState :: Board Field
iksOksInitialState = Board [Row [E, E, E], Row [E, E, E], Row [E, E, E]] P1

-- input: runGameStateOp applyMoves iksOksInitialState
-- output: 
-- (False,Board { board = [
--  | X | X | P | ,
--  | O | P | P | ,
--  | P | P | P | ], playerToMove = P2 } 
-- )
applyMoves :: GameStateOp (Board Field) Bool
applyMoves = do
  applyMove (Move (0, 1))
  applyMove (Move (1, 0))
  applyMove (Move (0, 0))


-- | Applies a move to the game board within a GameStateOpHistory context, updating the board's state and tracking history.
-- This function takes a single `Move` (defined by row and column indices) and returns a GameStateOpHistory.
-- When executed, it updates the game board based on the move, checks if this move concludes the game, 
-- and returns a tuple. The first element of the tuple is a boolean indicating whether the game has ended 
-- (True if it is over, False otherwise), and the second element is a list containing the new board state, 
-- capturing the history of board states.
-- This allows not only for the application of game logic but also for tracking the progression of the game over time.
-- Usage:
-- applyMoveH (Move (x, y)) - Applies the specified move to the board, updates its state, checks for game completion, 
-- and records the new state in the history.
applyMoveH :: Move -> GameStateOpHistory (Board Field) Bool
applyMoveH (Move (x, y)) = GameStateOpHistory (\board -> let newBoard = newStateAfterMove board (Move(x,y))
                                                         in (isGameOver newBoard, [newBoard]))

initialize :: GameStateOpHistory (Board Field) Bool
initialize = GameStateOpHistory (\states -> (False, [iksOksInitialState]))


-- runGameStateOpHistory applyMovesH iksOksInitialState
applyMovesH :: GameStateOpHistory (Board Field) Bool
applyMovesH = do
  initialize
  applyMoveH (Move (0, 1))
  applyMoveH (Move (1, 0))
  applyMoveH (Move (0, 0))

-- |X|O| |
-- | |X| |
-- | | | |
-- O (2,2)
-- X (1,0)
-- O (1,2)
-- x (0,2)

numOfPlayed:: [Row Field] -> Int
numOfPlayed [] = 0 
numOfPlayed rows =  foldl (\acc (Row field)-> numOfNonEmpty field + acc) 0 rows   


numOfNonEmpty:: [Field] -> Int
numOfNonEmpty xs = foldl (\acc x -> if x /= E then acc + 1 else acc) 0 xs





-- This module provides functionality for parsing game states and moves for a two-player board game 
-- (similar to Tic-Tac-Toe) from a text file, and applying those moves to the game board. It uses Parsec 
-- for parsing and defines a monadic structure (`GameStateOp`) for stateful operations on the game board. 
-- The game supports validating and applying a sequence of moves, checking for the game's completion, and 
-- toggling between two players. The `main` function demonstrates reading a game state and a series of moves 
-- from "stanje.txt", applying these moves to the initial board, and outputting the final board state or an 
-- error if the moves are invalid.

playerParser:: Parsec String () Player
playerParser = do spaces 
                  player <- letter
                  spaces
                  if player == 'X'
                    then return P1
                    else return P2
  
moveParser :: Parsec String () Move
moveParser = do
                spaces 
                letter
                spaces
                char '('
                spaces
                row <- digit
                spaces
                char ','
                spaces
                col <- digit
                spaces 
                char ')'
                spaces
                optional newline
                return (Move ( read [row], read [col]))

-- fieldParser :: Parsec String () Field
-- fieldParser = do  spaces
--                   field <- choice[letter <|> (char ' ')]
--                   case field of 'X' -> return X
--                                 'O' -> return O
--                                 ' ' -> return E
                              
fieldParser :: Parsec String () Field
fieldParser = choice [X <$ char 'X', O <$ char 'O', E <$ char ' ']


rowParser :: Parsec String () (Row Field)
rowParser = do
              char '|'
              r <- (endBy fieldParser (char '|'))
              optional newline
              return (Row r)



boardParser :: Parsec String () (Board Field)
boardParser = do
                 rows <- many1 rowParser
                 optional newline
                 let currentPlayer = if even (numOfPlayed rows) then P1 else P2
                 return (Board rows currentPlayer)


parseGameState :: Parsec String () (Board Field, [Move])
parseGameState = do
      spaces
      board <- boardParser
      spaces
      moves <- many1 moveParser
      return (board, moves)


applyMove' :: Move -> GameStateOp (Board Field) ( Maybe (Board Field))
applyMove' (Move (x, y)) = GameStateOp $ \board ->
                        let validMovesList = validMoves board
                            isGameOver' = isGameOver board 
                            in case (isGameOver', elem (x, y) validMovesList) of(True, _) -> (Just (board), board) 
                                                                                (_, False) -> (Nothing, board) 
                                                                                _ ->  let newBoard = newStateAfterMove board (Move (x, y))
                                                                                          isGameOver'' = isGameOver newBoard 
                                                                                          in (Just (newBoard), newBoard)


applyMoveFromFile :: [Move] -> GameStateOp (Board Field) (Maybe (Board Field))
applyMoveFromFile [] = do board <- GameStateOp $ \b -> (Just b, b)
                          return board
applyMoveFromFile (move : moves) = do result <- applyMove' move 
                                      case result of Nothing -> return Nothing
                                                     Just (board) -> applyMoveFromFile moves >>= return . fmap (const board)  -- Ovo obezbedjuje da se umesto GameStateOp (Board Field) ( Maybe (Board Field)) vrati Maybe (Board Field) 

main :: IO ()
main = do
  h <- readFile "file.txt"
  case runParser parseGameState () "" h of
    Right (board, moves) -> do
      let (res, updatedBoard) = runGameStateOp (applyMoveFromFile moves) board
      case res of
        Nothing -> print "Not valid"
        Just _ -> print updatedBoard
    Left x -> print x