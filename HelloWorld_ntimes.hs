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

hw :: String
hw = "Hello World" 

hello_worlds :: Int -> String
hello_worlds x = if x >= 1 
                    then hw ++ "\n" ++ (hello_worlds (x-1))
                 else ""

main :: IO()
main = do
    n <- readLn :: IO Int
    putStrLn (hello_worlds n)
    -- Print "Hello World" on a new line 'n' times.

