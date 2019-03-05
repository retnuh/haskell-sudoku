{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- remove after debugging
{-# LANGUAGE PartialTypeSignatures #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}



module Sudoku.Solvers.LensStateWriter where

import           Protolude
import           Control.Lens
import           Sudoku.Common
import           Sudoku.Solvers.Common
import           Sudoku.MessageQueue            ( MessageQueueType(..) )
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


data ContainerType = Row | Col | Box deriving (Show, Eq, Ord)

data Piece =
    Cell CellIndex
    | Container ContainerType Int
    deriving (Show, Eq, Ord)

data MessageType = IsValue | IsNotValue deriving (Show, Eq, Ord)

data Message = Message {
    _mtype :: MessageType,
    _recipient :: Piece,
    _sender :: Maybe Piece,
    _subject :: Piece,
    _value :: CellValue
}  deriving (Show, Ord)

instance Eq Message where
    Message { _mtype = aa, _subject = as, _recipient = ar, _value = av } == Message { _mtype = ba, _subject = bs, _recipient = br, _value = bv }
        = aa == ba && as == bs && ar == br && av == bv

data CellState =  CellState { _possibilities :: IntSet, _conts :: [Piece], _cellValue :: Maybe CellValue }
    deriving (Show, Eq, Ord)

data ContainerState = ContainerState {
    _activeCells :: Set Piece,
    _possibileCellsForValue :: IntMap (Set Piece)
} deriving (Show, Eq, Ord)

class (Monoid (q Message)) => MessageQueue (q :: * -> *) where
    next :: q Message -> Maybe (Message, q Message)
    len :: q Message -> Int

instance MessageQueue [] where
    next []       = Nothing
    next (x : xs) = Just (x, xs)
    len = Protolude.length

-- instance (forall a. Ord a) => MessageQueue Set a where
instance MessageQueue Set where
    next q = case Set.toAscList q of
        []       -> Nothing
        (x : xs) -> Just (x, Set.fromAscList xs)
    len = Set.size

instance MessageQueue DList where
    next q = case DList.toList q of
        []       -> Nothing
        (x : xs) -> Just (x, DList.fromList xs)
    len = Protolude.length . DList.toList

data GameState (q :: * -> *) = GameState {
    _wrapper :: [Message] -> q Message,
    _msgs :: MessageQueue q => q Message,
    _cells :: Map Piece CellState,
    _containers :: Map Piece ContainerState,
    _mstats :: MessageStats
}

makeLenses ''CellState
makeLenses ''ContainerState
makeLenses ''GameState

makeLenses ''PuzzleResults
makeLenses ''MessageStats

type MessageHandler q = (WriterT (q Message) (State (GameState q)))

initializeGameState
    :: (MessageQueue q) => MessageQueueWrapper q -> Puzzle -> GameState q
initializeGameState mqw p = GameState { _wrapper    = runWrapper mqw
                                      , _msgs       = msgs
                                      , _cells      = Map.fromList cells
                                      , _containers = Map.fromList containers
                                      , _mstats     = MessageStats 0 0
                                      }  where
    msgs  = runWrapper mqw $ initialMessages p
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
            IntMap.fromList $ zip [1 .. 9] $ replicate 9 (Set.fromList cells)


isFinished :: Maybe (Message, q Message) -> GameState q -> Bool
isFinished Nothing  gs = False
isFinished (Just _) gs = nullOf (cells . traverse . cellValue . _Nothing) gs

gamestateToPuzzle :: GameState q -> Puzzle
gamestateToPuzzle gs = aux <$> gs ^.. cells . traverse  where
    aux CellState { _cellValue = Just v } = v
    aux CellState{}                       = 0

filterSender :: Maybe Piece -> [Piece] -> [Piece]
filterSender Nothing  ps = ps
filterSender (Just p) ps = filter (/= p) ps

filteredContainers
    :: (MessageQueue q)
    => Lens' (GameState q) CellState
    -> Maybe Piece
    -> MessageHandler q [Piece]
filteredContainers cLens sender = uses (cLens . conts) (filterSender sender)

tellQ :: (MessageQueue q) => [Message] -> MessageHandler q ()
tellQ msgs = do
    wrap <- use wrapper
    tell $ wrap msgs

handleIsValueForCellRcpt :: (MessageQueue q) => Message -> MessageHandler q ()
handleIsValueForCellRcpt Message { _subject = self, _value = v, _sender = sender }
    = do
        -- we don't filter the sender b/c containers only send IsValue messages when handling a message about
        -- a different cell, so there is no worry of a cycle
        cs <- use (cells . singular (ix self) . conts)
        -- todo replace comprehension with lens fold cleverness?
        tellQ [ Message IsValue c (Just self) self v | c <- cs ]
        zoom (cells . singular (ix self)) $ do
            -- traceShowM ("set: " ++ show self ++ " val: " ++ show v ++ " sender: " ++ show sender)
            cellValue .= Just v
            possibilities .= IntSet.empty
            conts .= []

handleIsNotValueForCellRcpt
    :: (MessageQueue q) => Message -> MessageHandler q ()
handleIsNotValueForCellRcpt m@Message { _subject = self, _value = v, _sender = sender }
    = do
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
                        handleIsValueForCellRcpt m { _value = v }
                    else do
                        cs <- filteredContainers (cellLens self) sender
                        -- todo replace comprehension with lens fold cleverness?
                        tellQ
                            [ Message IsNotValue c (Just self) self v
                            | c <- cs
                            ]


removeCellAsPossibilityForValue
    :: (MessageQueue q) => Piece -> Piece -> Int -> MessageHandler q ()
removeCellAsPossibilityForValue cell self v = do
    let possLens :: Lens' (GameState q) (IntMap (Set Piece))
        possLens = containerLens self . possibileCellsForValue
    maybePoss <- use (possLens . singular (at v))
    case maybePoss of
        Nothing   -> return ()
        Just poss -> do
            let setLens :: Lens' (GameState q) (Set Piece)
                setLens = possLens . singular (ix v)
            let hadTwo = 2 == Set.size poss
            setLens . contains cell .= False
            oneLeft <- uses setLens ((== 1) . Set.size)
            when (hadTwo && oneLeft) $ do
                rcpt <- uses setLens (fromJust . head . toList)
                tellQ [Message IsValue rcpt (Just self) rcpt v]

handleIsValueForContainerRcpt
    :: (MessageQueue q) => Message -> MessageHandler q ()
handleIsValueForContainerRcpt Message { _subject = cell, _value = v, _recipient = self }
    = do
        zoom (containerLens self) $ do
            activeCells . contains cell .= False
            possibileCellsForValue . at v .= Nothing
        cells <- use $ containerLens self . activeCells
        tellQ [ Message IsNotValue c (Just self) c v | c <- Set.toList cells ]
        poss <- use (containerLens self . possibileCellsForValue)
        mapM_ (removeCellAsPossibilityForValue cell self) (IntMap.keys poss)


handleIsNotValueForContainerRcpt
    :: (MessageQueue q) => Message -> MessageHandler q ()
handleIsNotValueForContainerRcpt Message { _subject = cell, _value = v, _recipient = self }
    = removeCellAsPossibilityForValue cell self v

cellLens :: Piece -> Lens' (GameState q) CellState
cellLens p@(Cell _) = cells . singular (ix p)
-- this is partial, should be impossilbe to call with other pieces
-- todo figure out how to make impossible calls, you know, impossible
-- type families?
{-# INLINE cellLens #-}

containerLens :: Piece -> Lens' (GameState q) ContainerState
containerLens p = containers . singular (ix p)
-- this is partial, should be impossilbe to call with other pieces
-- todo figure out how to make impossible calls, you know, impossible
-- type families?
{-# INLINE containerLens #-}

handlerForMessage :: (MessageQueue q) => Message -> MessageHandler q ()
handlerForMessage m@Message { _mtype = mtype, _subject = sub, _recipient = rcpt, _sender = sender }
    = routeAction mtype rcpt
  where
    routeAction IsValue    c@(Cell _       ) = handleIsValueForCellRcpt m
    routeAction IsNotValue c@(Cell _       ) = handleIsNotValueForCellRcpt m
    routeAction IsValue    c@(Container _ _) = handleIsValueForContainerRcpt m
    routeAction IsNotValue c@(Container _ _) =
        handleIsNotValueForContainerRcpt m


runHandler
    :: (MessageQueue q)
    => MessageHandler q ()
    -> GameState q
    -> (q Message, GameState q)
runHandler = runState . execWriterT

runPuzzle :: (MessageQueue q) => State (GameState q) Puzzle
runPuzzle = do
    maybeNext <- uses msgs next
    finished  <- gets (isFinished maybeNext)
    if finished
        then do
            size <- uses msgs len
            mstats . remaining .= size
            gets gamestateToPuzzle
        else do
            let (m, ms) = fromJust maybeNext
            -- traceShowM ("msg:      " ++ show m)
            let handler = handlerForMessage m
            -- todo is there a state/lens method/operator that modifies and returns? 
            -- i.e. want messages from writer as result
            (newMsgs, newS) <- gets $ runHandler handler
            put newS
            -- let newMsgs' = [ trace ("    nmsgs: " ++ show m) m | m <- newMsgs ]
            mstats . used += 1
            -- yikes scary error, just do by hand
            -- msgs .= ms <> newMsgs
            modify (\gs -> gs { _msgs = ms <> newMsgs })
            runPuzzle


data LSWSolver q where
    LSWSolver :: MessageQueueType q -> Puzzle -> LSWSolver q

newtype MessageQueueWrapper (q :: * -> *) = MessageQueueWrapper { runWrapper :: (MessageQueue q) => ([Message] -> q Message) }

wrapAsSet :: MessageQueueWrapper Set
wrapAsSet = MessageQueueWrapper Set.fromList

derp :: MessageQueueType q -> MessageQueueWrapper q
-- derp _ = wrapAsSet
derp SetMQT   = MessageQueueWrapper Set.fromList
derp ListMQT  = MessageQueueWrapper identity
derp DListMQT = MessageQueueWrapper DList.fromList

instance (MessageQueue q) => Solver (LSWSolver q) where
    -- solve :: (MessageQueue q) => LSWSolver q -> PuzzleResults
    solve (LSWSolver mqt p) =
        let (sol, state) =
                    runState runPuzzle (initializeGameState (derp mqt) p)
        in  PuzzleResults { _complete = isComplete sol
                          , _correct  = isCorrect sol
                          , _solution = sol
                          , _stats    = _mstats state
                          }
