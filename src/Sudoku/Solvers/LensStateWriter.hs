{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sudoku.Solvers.LensStateWriter where

import           Protolude
import           Control.Lens
import           Sudoku.Common
import           Sudoku.Solvers.Common
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Control.Monad.State

type MessageQueue mt = [mt]

data Piece =
    Cell CellIndex
    | Row Int
    | Col Int | Box Int
    | Grid
    deriving (Show, Eq, Ord)

data MessageData =
    IsValue (Piece, CellValue)
    | IsNotValue
    deriving (Show, Eq, Ord)
makePrisms ''MessageData

data Message = Message { _msg :: MessageData, _sender :: Maybe Piece } deriving (Show, Ord)

instance Eq Message where
    Message { _msg = ad } == Message { _msg = bd } = ad == bd

makeLenses ''Message

data CellState =
    Value CellValue
    | Possibilities { _possibilities :: IntSet, _conts :: [Piece]}
    deriving (Show, Eq)
makePrisms ''CellState

data ContainerState = ContainerState {
    _activeCells :: Set Piece,
    _possibileCellsForValue :: IntMap [Piece]
} deriving (Show, Eq)
makeLenses ''ContainerState

data GameState = GameState {
    _msgCount :: Int,
    _msgs :: MessageQueue Message,
    _cells :: IntMap CellState,
    _containers :: Map Piece ContainerState
    } deriving (Show, Eq)

makeLenses ''GameState

initializeGameState :: Puzzle -> GameState
initializeGameState p = GameState { _msgCount   = 0
                                  , _msgs       = msgs
                                  , _cells      = cells
                                  , _containers = containers
                                  }  where
    msgs  = initialMessages p
    cells = IntMap.fromList $ zip [0 ..] $ makeCell <$> puzzleIndices
    containers =
        Map.fromList
            $   makeContainers
            =<< [ (Row, partitionRows)
                , (Col, partitionColumns)
                , (Box, partitionBoxes)
                ]

initialMessages :: Puzzle -> [Message]
initialMessages p = aux [] (zip puzzleIndices p)  where
    aux acc []            = acc
    aux acc ((_, 0) : vs) = aux acc vs
    aux acc ((i, v) : vs) =
        aux (Message (IsValue (Cell i, v)) Nothing : acc) vs

makeCell :: CellIndex -> CellState
makeCell ci = Possibilities
    { _possibilities = oneToNine
    , _conts         = [ Row (rowForCell ci)
                       , Col (columnForCell ci)
                       , Box (boxForCell ci)
                       ]
    }

makeContainers :: (Int -> Piece, Puzzle -> [[Int]]) -> [(Piece, ContainerState)]
makeContainers (ctor, pf) = mk <$> zip [0 .. 8] (pf puzzleIndices)
  where
    mk (i, cis) = (ctor i, ContainerState activeCells possibleCellsForValues)
      where
        cells       = Cell <$> cis
        activeCells = Set.fromList cells
        possibleCellsForValues =
            IntMap.fromList $ zip [1 .. 9] $ replicate 9 cells


isFinished :: GameState -> Bool
isFinished gs =
    nullOf (msgs . traverse) gs || nullOf (cells . traverse . _Possibilities) gs

gamestateToPuzzle :: GameState -> Puzzle
gamestateToPuzzle = undefined

runPuzzle :: State GameState Puzzle
runPuzzle = do
    finished <- gets isFinished
    if finished then gets gamestateToPuzzle else undefined
      -- pop first msg off queue, updating state

newtype LSWSolver = LSWSolver Puzzle

instance Solver LSWSolver where
    solve (LSWSolver p) =
        let (sol, state) = runState runPuzzle (initializeGameState p)
        in  PuzzleResults { complete          = isComplete sol
                          , correct           = isCorrect sol
                          , processedMessages = state ^. msgCount
                          , solution          = sol
                          }
