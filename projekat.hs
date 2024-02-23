import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import System.Environment
import System.IO
import Control.Monad
import Data.Monoid


-- PRVI DEO 

-- Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []]
-- (Node 5 [Node 2 [Node 1 [Node 3 [], Node 4 []] , Node 2 [] ] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []])
data Rose a = Node a [Rose a] deriving (Show)

size:: Rose a->Int
size (Node _ list) = 1 + sum (map (\elem->size elem) list) 

height:: Rose a->Int
height (Node _ []) = 0
height (Node _ list) = 1 + maximum (map (\elem->height elem) list)


leavesCount:: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ list) = sum (map (\elem->leavesCount elem) list)


leaves:: Rose a -> [a]
leaves (Node a []) = [a]
leaves (Node _ list) = concat( map (\elem->leaves elem) list)

elemsOnDepth:: Rose a -> Int -> [a]
elemsOnDepth (Node a _) 0 = [a]
elemsOnDepth (Node _ []) _ = []
elemsOnDepth (Node _ list) x = concat(map (\elem -> elemsOnDepth elem (x-1)) list)

-- fmap (+3) (Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []])
instance Functor Rose where
    fmap f (Node a list) = Node (f a) (map (\elem -> fmap f elem) list)


-- foldRose (+) 0 ( Node 5 [Node 2 [] , Node 3 [Node 4 [] , Node 8 []] , Node 7 []]) = 29
foldRose :: (a -> b -> b) -> b -> Rose a -> b
foldRose f acc (Node a list) = foldl (\acc' elem -> foldRose f acc' elem) (f a acc) list
    


-- DRUGI DEO

data Player = P1 | P2 
newtype Row a = Row [a] 
data Board a = Board { board :: [Row a], playerToMove :: Player } 
newtype Move = Move (Int, Int) 
data Field = X | O | E deriving Eq

-- DRUGI DEO

-- a -> (b , a )    =  trenutnoStanje -> ( rezultatOperacije , novoStanje )
newtype GameStateOp a b = GameStateOp { runGameStateOp :: a -> (b, a)} 


-- Uzimamo trenutno stanje, primnljujemo op na trenutno stanje i dobijamo rezultat operacije i novo stanje, na res primnjujemo f
instance Functor (GameStateOp s) where
  fmap f (GameStateOp op) = GameStateOp (\s -> let (res, newState) = op s in (f res, newState))



-- pure uzima vrednost i vraca tuple koji se sastoji od te vrednosti stanja, ne menja stanje
-- uzimamo state i sf primenjujemo na state odakle dobijamo funkciju i novi state, zatim smo primenili sa na novi state i dobili smo vrednost i novi state. Na kraju fn  primenimo na a
instance Applicative (GameStateOp s) where
  pure a = GameStateOp (\s -> (a, s))
  (GameStateOp sf) <*> (GameStateOp sa) = GameStateOp (\state -> let (fn,state1) = sf state
                                                                     (a, state2) = sa state1 in (fn a, state2))


-- h  je funckija od state monada
-- radimo dve stvari - prvo uzimamo izvrsavamo h na pocetno stanje zatim izvrsavamo f na a koje nam vraca novi monad i na kraju primenljujemo g na novo stanje
instance Monad (GameStateOp s) where
  return = pure
  (GameStateOp h) >>= f = GameStateOp $ \s -> let (a, newState) = h s
                                                  (GameStateOp g) = f a
                                                  in g newState


newtype GameStateOpHistory a b = GameStateOpHistory { runGameStateOpHistory :: a -> (b, [a]) }

instance Functor (GameStateOpHistory s) where
  fmap f (GameStateOpHistory op) = GameStateOpHistory (\states -> let (res, newStates) = op states in (f res, newStates))

instance Applicative (GameStateOpHistory s) where
  pure a = GameStateOpHistory (\s -> (a, [s]))
  (GameStateOpHistory sf) <*> (GameStateOpHistory sa) = GameStateOpHistory (\state -> let (fn, state1) = sf state
                                                                                          (a, state2) = sa (head state1)
                                                                                      in (fn a, state1 ++ state2))

instance Monad (GameStateOpHistory a) where
  return = pure
  (GameStateOpHistory h) >>= f = GameStateOpHistory (\s ->let (a, states) = h s 
                                                              GameStateOpHistory g = f a 
                                                              (b, newStates) = g (head states)
                                                           in (b, newStates ++ states))



-- TRECI DEO

-- ISPIS
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

-- instance Show a => Show (Row a) where
--         show :: Show a => Row a -> String
--         show (Row fields) = show fields

instance Show a => Show (Board a) where
  show (Board rows player) = "Board { board = " ++ show rows ++ ", playerToMove = " ++ show player ++ " } \n"
instance Show Move where
  show (Move (x,y)) = "(" ++ show x ++ ", "++show y ++ ")"

-- FUNCKIJE

-- validMoves (Board [Row [X,O,X], Row [X,X,X], Row[E,E,E] ] P2)
validMoves:: Board Field -> [(Int,Int)]
validMoves (Board rows _) = [(rowIndexs,colIndex) | (rowIndexs , row) <- zip [0..] rows, (colIndex, field) <- zip [0..] (rowFields row), field == E] 
            where rowFields (Row fields) = fields

-- boardToRows (Board [Row [X,O,X], Row [X,X,X], Row[E,E,E] ] P2)
boardToRows :: Board Field -> [Row Field]
boardToRows = board


-- Pomocna funkcija ista kao validMoves samo jos proverava da li je igra zavrsena
validMovesGameTree :: Board Field -> [(Int, Int)]
validMovesGameTree board
  | isGameOver board = []
  | otherwise = [(rowIndex, colIndex) | (rowIndex, row) <- zip [0..] rows, (colIndex, field) <- zip [0..] (rowFields row), field == E]
  where
    rows = boardToRows board
    rowFields (Row fields) = fields


-- Uzima Tablu sa redovima i bolje na koje treba da stavi znak, znak zavisi od trenutnog igraca
-- Uzimamo prvih i redova dodamo novi red i uzimamo sve redove posle i+1 reda
-- od reda u kom se stavlja novi element uzmemo prvih j polja na j+1 mesto stavimo znak i dodamo na to sva polja posle j+1 polja
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






testIsGameOver:: [Bool]
testIsGameOver = [isGameOver (Board [Row [X,O,X], Row [X,X,X], Row[E,E,E] ] P2) , isGameOver (Board [Row [X,O,X], Row [X,E,E], Row[X,E,E] ] P2), 
        isGameOver (Board [Row [X,O,X], Row [E,X,E], Row[E,E,X] ] P2),isGameOver (Board [Row [X,O,X], Row [E,X,E], Row[X,E,E] ] P2), isGameOver (Board [Row [X,O,X], Row [O,X,O], Row[O,X,O] ] P2), 
        isGameOver (Board [Row [E,E,E], Row [E,E,E], Row[E,E,E] ] P2)]

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




    -- getDiagonal1 :: [Row Field] -> [Field]
    -- getDiagonal1 rows = [getField (rows !! i) i | i <- [0..2]]
    -- getDiagonal2 :: [Row Field] -> [Field]
    -- getDiagonal2 rows = [getField (rows !! i ) (2 - i) | i <- [0..2]]
    -- getField :: Row Field -> Int -> Field
    -- getField (Row fields) index = fields !! index





-- createGameTree  (Board [Row [X,O,X], Row [X,X,O], Row[E,E,E] ] P2)
createGameTree :: Board Field -> Rose (Board Field)
createGameTree board = Node board children
  where
    currentPlayer = playerToMove board
    moves = validMovesGameTree board
    children = map (\move -> createGameTree (newStateAfterMove board (Move move))) moves

-- CETVRTI DEO


applyMove :: Move -> GameStateOp (Board Field) Bool
applyMove (Move (x, y)) = GameStateOp $ \board ->
  let newBoard = newStateAfterMove board (Move(x,y))
  in (isGameOver newBoard, newBoard)

-- runGameStateOp applyMoves iksOksInitialState
applyMoves :: GameStateOp (Board Field) Bool
applyMoves = do
  applyMove (Move (0, 1))
  applyMove (Move (1, 0))
  applyMove (Move (0, 0))


applyMoveH :: Move -> GameStateOpHistory (Board Field) Bool
applyMoveH (Move (x, y)) = GameStateOpHistory (\board -> let newBoard = newStateAfterMove board (Move(x,y))
                                                         in (isGameOver newBoard, [newBoard]))

iksOksInitialState :: Board Field
iksOksInitialState = Board [Row [E, E, E], Row [E, E, E], Row [E, E, E]] P1

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
  h <- readFile "stanje.txt"
  case runParser parseGameState () "" h of
    Right (board, moves) -> do
      let (res, updatedBoard) = runGameStateOp (applyMoveFromFile moves) board
      case res of
        Nothing -> print "Not valid"
        Just _ -> print updatedBoard
    Left x -> print x