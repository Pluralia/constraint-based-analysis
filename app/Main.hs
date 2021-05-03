module Main
    ( main
    ) where

import qualified Data.Map.Strict as Map
import           Text.Read

import           Debug.Trace

import           Analyze.Simple
import           Evaluate.Simple
import           Common
import           Example.Simple
import           Types

----------------------------------------------------------------------------------------------------
-- for tests

main :: IO ()
main = do
--  handleInput twoIdApp
  handleInput oneVarTwoLam
--  handleInput contextDep

----------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
  putStrLn "Please, enter an expression"
  input <- getLine    -- (L "x" (V "x")) :@: (L "y" (V "y"))
  maybe main handleInput ((readMaybe input) :: Maybe Exp)
-}

handleInput :: Exp -> IO ()
handleInput exp = do
  let res = analyzeDArray exp
  mapM_ print . Map.toList $ res
  print "---------------------------------------------"
  let checkRes = evaluateDArray exp
  mapM_ print . Map.toList $ res
  print "---------------------------------------------"
  print $ res == checkRes

----------------------------------------------------------------------------------------------------

