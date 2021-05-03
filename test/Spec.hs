module Main
    ( main
    ) where

import           Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Analyze.Simple
import           Common
import           Evaluate.Simple
import qualified Example.Simple  as Simple
import           Types

-----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  hspec $ do
    describe "Simple" $ do
      it "2 id application" $ do
        go Simple.twoIdApp

      it "sequential application" $ do
        go Simple.seqApp

      it "2 lambdas for 1 variable" $ do
        go Simple.oneVarTwoLam

      it "CBA0 fails <-- fails by `Evaluate.Simple.evaluateDArray`" $ do
        analyzeDArray Simple.contextDep `shouldBe` expectedByContextDep 

      it "not nf argument which is not used" $ do
        go Simple.unusedNotNfArg

      it "infinite evaluation" $ do
        go Simple.y_comb

----------------------------------------------------------------------------------------------------

go :: Exp -> Expectation
go exp = analyzeDArray exp `shouldBe` evaluateDArray exp

expectedByContextDep :: DArray
expectedByContextDep = Map.fromList [
  (C(1),Set.fromList [Lam 9 (Abst "x" (Var 8 "x"))]),
  (C(2),Set.fromList [Lam 9 (Abst "x" (Var 8 "x"))]),
  (C(3),Set.fromList [Lam 5 (Abst "y" (Var 4 "y")), Lam 9 (Abst "x" (Var 8 "x"))]),
  (C(4),Set.fromList [Lam 5 (Abst "y" (Var 4 "y"))]),
  (C(5),Set.fromList [Lam 5 (Abst "y" (Var 4 "y"))]),
  (C(6),Set.fromList [Lam 5 (Abst "y" (Var 4 "y")),Lam 9 (Abst "x" (Var 8 "x"))]),
  (C(7),Set.fromList [Lam 7 (Abst "f" (App 6 (App 3 (Var 1 "f") (Var 2 "f")) (Lam 5 (Abst "y" (Var 4 "y")))))]),
  (C(8),Set.fromList [Lam 5 (Abst "y" (Var 4 "y")),Lam 9 (Abst "x" (Var 8 "x"))]),
  (C(9),Set.fromList [Lam 9 (Abst "x" (Var 8 "x"))]),
  (C(10),Set.fromList [Lam 5 (Abst "y" (Var 4 "y")),Lam 9 (Abst "x" (Var 8 "x"))]),
  (R "f",Set.fromList [Lam 9 (Abst "x" (Var 8 "x"))]),
  (R "x",Set.fromList [Lam 5 (Abst "y" (Var 4 "y")),Lam 9 (Abst "x" (Var 8 "x"))]),
  (R "y",Set.fromList [Lam 5 (Abst "y" (Var 4 "y"))])]

----------------------------------------------------------------------------------------------------

