{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
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
    _subject :: Piece,
    _recipient :: Piece,
    _value :: CellValue,
    _sender :: Maybe Piece
    }  deriving (Show, Ord)

instance Eq Message where
    Message { _action = aa, _subject = as, _recipient = ar, _value = av } == Message { _action = ba, _subject = bs, _recipient = br, _value = bv }
        = aa == ba && as == bs && ar == br && av == bv

data CellState =
    Value CellValue
    | Possibilities { _possibilities :: IntSet, _conts :: [Piece]}
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

makePrisms ''CellState
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
    aux acc ((i, v) : vs) = aux
        ( Message { _action    = IsValue
                  , _value     = v
                  , _subject   = Cell i
                  , _recipient = Cell i
                  , _sender    = Nothing
                  }
        : acc
        )
        vs

makeCell :: CellIndex -> (Piece, CellState)
makeCell ci =
    ( Cell ci
    , Possibilities
        { _possibilities = oneToNine
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
    nullOf (msgs . traverse) gs || nullOf (cells . traverse . _Possibilities) gs

gamestateToPuzzle :: GameState -> Puzzle
gamestateToPuzzle gs = aux <$> gs ^.. cells . traverse  where
    aux (Value v)       = v
    aux Possibilities{} = 0

filterSender :: Maybe Piece -> [Piece] -> [Piece]
filterSender Nothing  ps = ps
filterSender (Just p) ps = filter (/= p) ps

filteredContainers
    :: Lens' GameState CellState -> Maybe Piece -> MessageHandler [Piece]
filteredContainers cellLens sender =
    uses (cellLens . _Possibilities . _2) (filterSender sender)

handleIsValueForCellRcpt
    :: Message -> Lens' GameState CellState -> MessageHandler ()
handleIsValueForCellRcpt Message { _subject = self, _value = v, _sender = sender } cellLens
    = do
        cs <- filteredContainers cellLens sender
        tell -- todo replace comprehension with lens fold cleverness?
            [ Message { _action    = IsValue
                      , _subject   = self
                      , _recipient = c
                      , _sender    = Just self
                      , _value     = v
                      }
            | c <- cs
            ]
        cellLens .= Value v

handleIsNotValueForCellRcpt
    :: Message -> Lens' GameState CellState -> MessageHandler ()
handleIsNotValueForCellRcpt m@Message { _subject = self, _value = v, _sender = sender } cellLens
    = return ()
    -- = let possLens = cellLens . _Possibilities . _1
    --   in
    --       let containedLens = possLens . contains v
    --       in
    --           do
    --               contained <- use containedLens
    --               if not contained
    --                   then return ()
    --                   else do
    --                       containedLens .= False
    --                       size <- uses possLens IntSet.size
    --                       if True
    --                           then do
    --                               (newMsgs, newS) <-
    --                                   gets
    --                                       (runHandler
    --                                           (handleIsValueForCellRcpt
    --                                               m
    --                                               cellLens
    --                                           )
    --                                       )
    --                               put newS
    --                               tell newMsgs
    --                               return ()
    --                           else do
    --                               cs <- filteredContainers cellLens sender
    --                               tell
    --                                   [ Message { _action    = IsValue
    --                                             , _subject   = self
    --                                             , _recipient = c
    --                                             , _sender    = Just self
    --                                             , _value     = v
    --                                             }
    --                                   | c <- cs
    --                                   ]
    --                               cellLens .= Value v
    --                               return ()



handleIsValueForContainerRcpt
    :: Message -> Lens' GameState ContainerState -> MessageHandler ()
handleIsValueForContainerRcpt Message { _subject = cell, _value = v, _sender = sender, _recipient = self } lens
    = return ()

cellLens :: Piece -> Lens' GameState CellState
cellLens p = cells . singular (ix p)

containerLens :: Piece -> Lens' GameState ContainerState
containerLens p = containers . singular (ix p)

handlerForMessage :: Message -> MessageHandler ()
handlerForMessage m@Message { _action = act, _subject = sub, _recipient = rcpt, _sender = sender }
    = routeAction act rcpt
  where
    routeAction IsValue c@(Cell _) = handleIsValueForCellRcpt m (cellLens c)
    routeAction IsNotValue c@(Cell _) =
        handleIsNotValueForCellRcpt m (cellLens c)
    routeAction IsValue c@(Container _ _) =
        handleIsValueForContainerRcpt m (containerLens c)

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

testW :: (WriterT String (State Int)) ()
testW = do
    modify (+ 1)
    tell "foo "
    modify (+ 3)
    tell "bar"
    return ()

-- goW = runState (runWriterT testW "start ") 5
