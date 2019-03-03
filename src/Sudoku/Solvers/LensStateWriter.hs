{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- remove after debugging
{-# LANGUAGE PartialTypeSignatures #-}


module Sudoku.Solvers.LensStateWriter where

import           Protolude
import           Control.Lens
import           Sudoku.Common
import           Sudoku.Solvers.Common
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.IntSet                   as IntSet
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Control.Monad.State
import           Control.Monad.Writer

-- remove these after debugging
import qualified Sudoku.Puzzles                as Puzzles
import           Prelude                        ( String )

type MessageQueue mt = [mt]

data ContainerType = Row | Col | Box deriving (Show, Eq, Ord)

data Piece =
    Cell CellIndex
    | Container ContainerType Int
    deriving (Show, Eq, Ord)

data ActionName = IsValue | IsNotValue deriving (Show, Eq, Ord)

data Message = Message {
    _action :: ActionName,
    _recipient :: Piece,
    _sender :: Maybe Piece,
    _subject :: Piece,
    _value :: CellValue
    }  deriving (Show, Ord)

instance Eq Message where
    Message { _action = aa, _subject = as, _recipient = ar, _value = av } == Message { _action = ba, _subject = bs, _recipient = br, _value = bv }
        = aa == ba && as == bs && ar == br && av == bv

data CellState =  CellState { _possibilities :: IntSet, _conts :: [Piece], _cellValue :: Maybe CellValue }
    deriving (Show, Eq, Ord)

data ContainerState = ContainerState {
    _activeCells :: Set Piece,
    _possibileCellsForValue :: IntMap [Piece]
} deriving (Show, Eq, Ord)

data GameState = GameState {
    _msgCount :: Int,
    _msgs :: MessageQueue Message,
    _cells :: Map Piece CellState,
    _containers :: Map Piece ContainerState
} deriving (Show, Eq, Ord)

makeLenses ''CellState
makeLenses ''ContainerState
makeLenses ''GameState

type MessageHandler = (WriterT [Message] (State GameState))

initializeGameState :: Puzzle -> GameState
initializeGameState p = GameState { _msgCount   = 0
                                  , _msgs       = msgs
                                  , _cells      = Map.fromList cells
                                  , _containers = Map.fromList containers
                                  }  where
    msgs  = initialMessages p
    -- states = Map.fromList (cells ++ containers)
    cells = makeCell <$> puzzleIndices
    containers =
        makeContainers
            =<< [ (Row, partitionRows)
                , (Col, partitionColumns)
                , (Box, partitionBoxes)
                ]

initialMessages :: Puzzle -> [Message]
initialMessages p = aux [] (zip puzzleIndices p)  where
    aux acc []            = acc
    aux acc ((_, 0) : vs) = aux acc vs
    aux acc ((i, v) : vs) =
        aux (Message IsValue (Cell i) Nothing (Cell i) v : acc) vs

makeCell :: CellIndex -> (Piece, CellState)
makeCell ci =
    ( Cell ci
    , CellState
        { _possibilities = oneToNine
        , _cellValue     = Nothing
        , _conts         = [ Container Row (rowForCell ci)
                           , Container Col (columnForCell ci)
                           , Container Box (boxForCell ci)
                           ]
        }
    )

makeContainers
    :: (ContainerType, Puzzle -> [[Int]]) -> [(Piece, ContainerState)]
makeContainers (t, pf) = mk <$> zip [0 .. 8] (pf puzzleIndices)
  where
    mk (i, cis) =
        (Container t i, ContainerState activeCells possibleCellsForValues)
      where
        cells       = Cell <$> cis
        activeCells = Set.fromList cells
        possibleCellsForValues =
            IntMap.fromList $ zip [1 .. 9] $ replicate 9 cells


isFinished :: GameState -> Bool
isFinished gs =
    nullOf (msgs . traverse) gs
        || nullOf (cells . traverse . cellValue . _Nothing) gs

gamestateToPuzzle :: GameState -> Puzzle
gamestateToPuzzle gs = aux <$> gs ^.. cells . traverse  where
    aux CellState { _cellValue = Just v } = v
    aux CellState{}                       = 0

filterSender :: Maybe Piece -> [Piece] -> [Piece]
filterSender Nothing  ps = ps
filterSender (Just p) ps = filter (/= p) ps

filteredContainers
    :: Lens' GameState CellState -> Maybe Piece -> MessageHandler [Piece]
filteredContainers cellLens sender =
    uses (cellLens . conts) (filterSender sender)

handleIsValueForCellRcpt :: Message -> MessageHandler ()
handleIsValueForCellRcpt Message { _subject = self, _value = v, _sender = sender }
    = do
        cs <- filteredContainers (cells . singular (ix self)) sender
        -- todo replace comprehension with lens fold cleverness?
        tell [ Message IsValue c (Just self) self v | c <- cs ]
        zoom (cells . singular (ix self)) $ do
            cellValue .= Just v
            possibilities .= IntSet.empty
            conts .= []

handleIsNotValueForCellRcpt :: Message -> MessageHandler ()
handleIsNotValueForCellRcpt m@Message { _subject = self, _value = v, _sender = sender }
    = do
        stillPossible <- use (cellLens self . possibilities . contains v)
        if not stillPossible
            then return ()
            else do
                (cellLens self . possibilities . contains v) .= False
                size <- uses (cellLens self . possibilities) IntSet.size
                if size == 1
                    then handleIsValueForCellRcpt m
                    else do
                        cs <- filteredContainers (cellLens self) sender
                        -- todo replace comprehension with lens fold cleverness?
                        tell [ Message IsValue c (Just self) self v | c <- cs ]


handleIsValueForContainerRcpt :: Message -> MessageHandler ()
handleIsValueForContainerRcpt Message { _subject = cell, _value = v, _sender = sender, _recipient = self }
    = return ()

cellLens :: Piece -> Lens' GameState CellState
cellLens p@(Cell _) = cells . singular (ix p)

containerLens :: Piece -> Lens' GameState ContainerState
containerLens p@(Container _ _) = containers . singular (ix p)

handlerForMessage :: Message -> MessageHandler ()
handlerForMessage m@Message { _action = act, _subject = sub, _recipient = rcpt, _sender = sender }
    = routeAction act rcpt
  where
    routeAction IsValue c@(Cell _       ) = handleIsValueForCellRcpt m
    -- routeAction IsNotValue c@(Cell _) =
    --     handleIsNotValueForCellRcpt m 
    routeAction IsValue c@(Container _ _) = handleIsValueForContainerRcpt m

runHandler :: MessageHandler () -> GameState -> ([Message], GameState)
runHandler = runState . execWriterT

runPuzzle :: State GameState Puzzle
runPuzzle = do
    finished <- gets isFinished
    if finished
        then gets gamestateToPuzzle
        else do
            ~(m : ms) <- use msgs
            let handler = handlerForMessage m
            (newMsgs, newS) <- gets $ runHandler handler
            -- todo is there a state/lens method/operator that modifies and returns? 
            -- i.e. want messages from writer as result
            put newS
            msgs .= ms ++ newMsgs
            msgCount += 1
            runPuzzle


newtype LSWSolver = LSWSolver Puzzle

instance Solver LSWSolver where
    solve (LSWSolver p) =
        let (sol, state) = runState runPuzzle (initializeGameState p)
        in  PuzzleResults { complete          = isComplete sol
                          , correct           = isCorrect sol
                          , processedMessages = state ^. msgCount
                          , solution          = sol
                          }

test7 = do
    modify (+ 1)
    lift $ modify (++ "1")
    a <- get
    b <- lift get
    return (a, b)

go7 = evalState (evalStateT test7 0) "0"

testW2 :: (WriterT String (State Int)) Int
testW2 = do
    modify (* 10)
    tell " w2 "
    return 69

testW :: (WriterT String (State Int)) ()
testW = do
    modify (+ 1)
    tell "foo "
    testW2
    modify (+ 3)
    tell "bar"
    return ()

goW = runState (runWriterT testW) 4
