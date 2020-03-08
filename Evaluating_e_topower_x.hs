{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

fa :: Int -> Int
fa t = if (t == 0) 
            then 1
            else t * (fa (t-1))

po :: Double -> Int -> Double
po x t = if(t == 0) 
            then 1
            else x * (po x (t-1))
po _ 0 = 1

f :: Double -> Int -> Double
f x t = if (t < 10) 
            then (po x t)/(fromIntegral (fa t)) + f x (t+1)
            else 0

main :: IO()
main = do
    n <- readLn :: IO Int

    forM_ [1..n] $ \n_itr -> do
        x <- readLn :: IO Double
        print (f x 0)
