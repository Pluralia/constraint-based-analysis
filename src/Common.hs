module Common where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Types

----------------------------------------------------------------------------------------------------

makeLabel :: Exp -> Term
makeLabel = fst . go 1
  where
    go :: Lab -> Exp -> (Term, Lab)
    go label (V name)        = (Var label name, succ label)
    go label (lexp :@: rexp) =
      let (lterm, llabel) = go label lexp
          (rterm, rlabel) = go llabel rexp
       in (App rlabel lterm rterm, succ rlabel)
    go label (L name exp) =
      let (term, updLabel) = go label exp
       in (Lam updLabel (Abst name term), succ updLabel)

----------------------------------------------------------------------------------------------------

