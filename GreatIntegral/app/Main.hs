{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Stream
import Data.Functor.Identity
import Data.Array as Arr
import Data.Array.ST as ArrST
import Control.Monad
import Data.List as L
import MyLib
import Data.Array.Base (IArray)
import Control.Monad.Trans.State as StT
import Control.Monad.State as St
import Control.Monad.Except as Ex
import Control.Monad.Trans.Except as ExT
import Control.Monad.Logic as Logic
import Control.Applicative
import Control.Monad.Trans.Reader

data GameState = X | O | N deriving (Eq, Ord, Show, Enum, Read)

kk :: Int -> Array (Int, Int) GameState
kk x = array ((1, 1), (x, x)) [((i, i2), N) | i <- [1..x], i2 <- [1..x]]

kk3 :: Array (Int, Int) GameState
kk3 = listArray ((1,1),(3,3)) 
    [X,X,X,
     O,X,X,
     O,O,X]

kk4 :: Array (Int, Int) GameState
kk4 = listArray ((1,1),(3,3)) 
    [N,N,O,
     N,N,O,
     N,N,O]


arrCols :: (Ix b, Enum b) => Array (b, b) e -> [[e]]
arrCols array = 
    transpose $ (fmap.fmap) (array Arr.!) $ colBoundGen array
    where
        colBoundGen (Arr.bounds -> ((a,b),(c,d)))
            = fmap2 (,) [b..d] [a..c]

arrRows :: (Ix a, Enum a) => Array (a, a) e -> [[e]]
arrRows array = 
    (fmap.fmap)  (array Arr.!) $ rowBoundGen array
    where
        rowBoundGen (Arr.bounds -> ((a,b),(c,d)) )
            = fmap2 (,) [a..c] [b..d]

arrDiag :: (Ix a, Enum a) => Array (a, a) e -> [e]
arrDiag array = 
    fmap (array Arr.!) $ boundGen array
    where
        boundGen (Arr.bounds -> ((a1,b1),(c1,d1))) 
            = L.zipWith (,) [a1..c1] [b1..d1]

arrDiagR :: (Ix a, Enum a) => Array (a, a) e -> [e]
arrDiagR array = 
    fmap (array Arr.!) $ boundGen array
    where
        boundGen (Arr.bounds -> ((a1,b1),(c1,d1))) 
            = (\x -> L.zipWith (,) x (L.reverse x)) [a1..c1]


pPL :: (Foldable t, Show a) => t a -> IO ()
pPL list = mapM_ print list

pP :: Show e => Array (Int,Int) e -> IO ()
pP array = pPL $ arrRows array

mutArray :: (Ix a1, Ix b, IArray a2 e) => a2 (a1, b) e -> (a1, b) -> e -> Array (a1, b) e
mutArray x (x1,y1) newVal = runSTArray $ do
    x2 <- ArrST.thaw x
    writeArray x2 (x1,y1) newVal
    return x2

choose :: Foldable f => f a -> LogicT m a
choose = (foldr ((<|>). pure) empty)

type Arry = Array (Int,Int) GameState
    
interpret :: forall e. Read e => [Char] -> Array (Int,Int) e -> Array (Int,Int) e
interpret list array =
    (\list -> 
        case list of
            [a,b,c] -> mutArray array (read @Int a, read @Int b) (read @e c)
            _  -> array)
        (parts (\x -> (==) ',' x)  [] list)

gameLoop :: StateT Arry (ExceptT (IO ()) IO) ()
gameLoop = do
    input <- (lift.lift) getLine 
    input2 <- (lift.lift) getLine 
    St.modify (interpret input)
    St.modify (interpret input2)
    currBoard <- St.get
    (lift.lift) $ pP (currBoard)
    case checkWin currBoard of
        (True, Just x) -> (lift.lift) $ putStrLn $ "Player " <> show x <> " won!"
        _              -> gameLoop


allSame :: [GameState] -> (Bool, Maybe GameState)
allSame [] = (True,Nothing)
allSame (x:xs) = 
    case lok of
        True -> (True, Just x)
        False -> (False, Nothing)
    where lok = (all (== x) xs) && (x /= N)

cmpList :: [a] -> [b] -> Ordering
cmpList (x:xs) (x2:xs2) = cmpList xs xs2
cmpList [] []  = EQ
cmpList [] list = LT
cmpList list [] = GT

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

only1Same :: [[GameState]] -> (Bool, Maybe GameState)
only1Same x = 
    let 
        filtList = L.filter (\x -> fst $ allSame x) x
        in
            case (cmpList filtList [()]) of
                GT -> (False,Nothing)
                EQ -> (True, (safeHead>=>safeHead) filtList)
                LT -> (False,Nothing)

checkWin :: Array (Int,Int) GameState -> (Bool, Maybe GameState)
checkWin array = 
    let
        allRows  = only1Same $ arrRows array
        allCols  = only1Same $ arrCols array
        allDiag  = allSame $ arrDiag array
        allDiagR = allSame $ arrDiagR array
        apt1 (a,b) (c,d) = (a^^^c,b<|>d)
        in
         case (allDiag `apt1` allDiagR) `apt1` (allRows `apt1` allCols) of
                (False, _) -> (False, Nothing)
                (True, x)  -> (True, x)


checkWinS array = 
    let
        allRows  = only1Same $ arrRows array
        allCols  = only1Same $ arrCols array
        allDiag  = allSame $ arrDiag array
        allDiagR = allSame $ arrDiagR array
        apt1 (a,b) (c,d) = (a^^^c,b<|>d)
        in
         (
         allRows,
         allCols,
         allDiag,
         allDiagR)

runGameLoop = runExceptT (runStateT gameLoop (kk 3))
runGameLoop2 = runExceptT (runStateT gameLoop (kk4))

main = runGameLoop