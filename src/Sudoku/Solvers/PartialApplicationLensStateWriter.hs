{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sudoku.Solvers.PartialApplicationLensStateWriter where 

import           Protolude
import           Control.Lens
import           Sudoku.Common
import           Sudoku.Solvers.Common
import           Sudoku.MessageQueue
import           Data.Maybe                     ( fromJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.IntSet                   as IntSet
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.DList                     ( DList )
import qualified Data.DList                    as DList
import           Control.Monad.State
import           Control.Monad.Writer

newtype Cell = Cell CellIndex deriving (Show, Eq, Ord)

data ContainerType = Row | Col | Box deriving (Show, Eq, Ord)
data Container = Container ContainerType Int deriving (Show, Eq, Ord)


data MessageType = IsValue | IsNotValue deriving (Show, Eq, Ord)

type MessageHandler = (WriterT [Message] (State GameState))

data Message = Message {
    mtype :: MessageType,
    handler :: MessageHandler ()
}

instance Eq Message where
    Message{} == Message{} = False

instance Ord Message where
    compare Message { mtype = at } Message { mtype = bt } =
        if at <= bt then LT else GT

data CellState =  CellState { _possibilities :: IntSet, _conts :: [Container], _cellValue :: Maybe CellValue }
    deriving (Show, Eq, Ord)

data ContainerState = ContainerState {
    _activeCells :: Set Cell,
    _possibileCellsForValue :: IntMap (Set Cell)
} deriving (Show, Eq, Ord)

data MessageState q = MessageState { _msgs :: (MessageQueue q Message) => q Message
                                   , _mstats :: MessageStats
                                   }

data GameState = GameState { _cells :: Map Cell CellState
                           , _containers :: Map Container ContainerState
                           }

makeLenses ''CellState
makeLenses ''ContainerState
makeLenses ''MessageState
makeLenses ''GameState

makeLenses ''PuzzleResults
makeLenses ''MessageStats

initializeGameState
    :: (MessageQueue q Message)
    => ([Message] -> q Message)
    -> Puzzle
    -> (MessageState q, GameState)
initializeGameState wrapper p =
    ( MessageState { _msgs = msgs, _mstats = MessageStats 0 0 }
    , GameState { _cells      = Map.fromList cells
                , _containers = Map.fromList containers
                }
    )  where
    msgs  = wrapper $ initialMessages p
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
        aux (Message IsValue (handleIsValueForCellRcpt (Cell i) v) : acc) vs

makeCell :: CellIndex -> (Cell, CellState)
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
    :: (ContainerType, Puzzle -> [[Int]]) -> [(Container, ContainerState)]
makeContainers (t, pf) = mk <$> zip [0 .. 8] (pf puzzleIndices)
  where
    mk (i, cis) =
        (Container t i, ContainerState activeCells possibleCellsForValues)
      where
        cells       = Cell <$> cis
        activeCells = Set.fromList cells
        possibleCellsForValues =
            IntMap.fromList $ zip [1 .. 9] $ replicate 9 (Set.fromList cells)


isFinished
    :: (MessageQueue q Message)
    => Maybe (Message, q Message)
    -> GameState
    -> Bool
isFinished Nothing  gs = True
isFinished (Just _) gs = nullOf (cells . traverse . cellValue . _Nothing) gs

gamestateToPuzzle :: GameState -> Puzzle
gamestateToPuzzle gs = aux <$> gs ^.. cells . traverse  where
    aux CellState { _cellValue = Just v } = v
    aux CellState{}                       = 0

filteredContainers
    :: Lens' GameState CellState -> Container -> MessageHandler [Container]
filteredContainers cLens sender = uses (cLens . conts) (filter (/= sender))

handleIsValueForCellRcpt :: Cell -> CellValue -> MessageHandler ()
handleIsValueForCellRcpt self v = do
        -- we don't filter the sender b/c containers only send IsValue messages when handling a message about
        -- a different cell, so there is no worry of a cycle
    cs <- use (cells . singular (ix self) . conts)
    tell [ Message IsValue (handleIsValueForContainerRcpt self v c) | c <- cs ]
    zoom (cells . singular (ix self)) $ do
        cellValue .= Just v
        possibilities .= IntSet.empty
        conts .= []

handleIsNotValueForCellRcpt
    :: Cell -> CellValue -> Container -> MessageHandler ()
handleIsNotValueForCellRcpt self v sender = do
    stillPossible <- use (cellLens self . possibilities . contains v)
    if not stillPossible
        then return ()
        else do
            (cellLens self . possibilities . contains v) .= False
            size <- uses (cellLens self . possibilities) IntSet.size
            if size == 1
                then do
                    v <- uses (cellLens self . possibilities)
                              (fromJust . head . IntSet.toList)
                    handleIsValueForCellRcpt self v
                else do
                    cs <- filteredContainers (cellLens self) sender
                    tell
                        [ Message
                              IsNotValue
                              (handleIsNotValueForContainerRcpt self v c)
                        | c <- cs
                        ]


removeCellAsPossibilityForValue
    :: Cell -> CellValue -> Container -> MessageHandler ()
removeCellAsPossibilityForValue cell v self = do
    let possLens :: Lens' GameState (IntMap (Set Cell))
        possLens = containerLens self . possibileCellsForValue
    maybePoss <- use (possLens . singular (at v))
    case maybePoss of
        Nothing   -> return ()
        Just poss -> do
            let setLens :: Lens' GameState (Set Cell)
                setLens = possLens . singular (ix v)
            let hadTwo = 2 == Set.size poss
            setLens . contains cell .= False
            oneLeft <- uses setLens ((== 1) . Set.size)
            when (hadTwo && oneLeft) $ do
                rcpt <- uses setLens (fromJust . head . toList)
                tell [Message IsValue (handleIsValueForCellRcpt rcpt v)]

handleIsValueForContainerRcpt
    :: Cell -> CellValue -> Container -> MessageHandler ()
handleIsValueForContainerRcpt cell v self = do
    zoom (containerLens self) $ do
        activeCells . contains cell .= False
        possibileCellsForValue . at v .= Nothing
    cells <- use $ containerLens self . activeCells
    tell
        [ Message IsNotValue (handleIsNotValueForCellRcpt cell v self)
        | cell <- Set.toList cells
        ]
    poss <- use (containerLens self . possibileCellsForValue)
    mapM_ (\v -> removeCellAsPossibilityForValue cell v self) (IntMap.keys poss)

handleIsNotValueForContainerRcpt
    :: Cell -> CellValue -> Container -> MessageHandler ()
handleIsNotValueForContainerRcpt = removeCellAsPossibilityForValue

cellLens :: Cell -> Lens' GameState CellState
cellLens p = cells . singular (ix p)
{-# INLINE cellLens #-}

containerLens :: Container -> Lens' GameState ContainerState
containerLens p = containers . singular (ix p)
{-# INLINE containerLens #-}

runHandler :: MessageHandler () -> GameState -> ([Message], GameState)
runHandler = runState . execWriterT

runPuzzle
    :: (MessageQueue q Message)
    => ([Message] -> q Message)
    -> State (MessageState q, GameState) Puzzle
runPuzzle mqw = do
    maybeNext <- uses (_1 . msgs) next
    finished  <- gets (isFinished maybeNext . snd)
    if finished
        then do
            size <- uses (_1 . msgs) len
            _1 . mstats . remaining .= size
            gets (gamestateToPuzzle . snd)
        else do
            let (m, ms) = fromJust maybeNext
            -- traceShowM ("msg:      " ++ show m)
            -- todo is there a state/lens method/operator that modifies and returns? 
            -- i.e. want messages from writer as result
            (newMsgs, newS) <- gets $ runHandler (handler m) . snd
            _2 .= newS
            _1 . mstats . used += 1
            -- yikes scary error, just do by hand
            -- _1 . msgs .= ms <> mqw newMsgs
            modify (\(mst, gst) -> (mst { _msgs = ms <> mqw newMsgs }, gst))
            -- let newMsgs' = [ trace ("    nmsgs: " ++ show m) m | m <- newMsgs ]
            runPuzzle mqw


newtype PartialApplicationLSWSolver = PartialApplicationLSWSolver Puzzle

instance Solver PartialApplicationLSWSolver where
    type Msg PartialApplicationLSWSolver = Message
    solve
        :: (MessageQueue q Message)
        => ([Message] -> q Message)
        -> PartialApplicationLSWSolver
        -> PuzzleResults
    solve mqw (PartialApplicationLSWSolver p) =
        let (sol, (mstate, gstate)) =
                    runState (runPuzzle mqw) (initializeGameState mqw p)
        in  PuzzleResults { _complete = isComplete sol
                          , _correct  = isCorrect sol
                          , _solution = sol
                          , _stats    = _mstats mstate
                          }
