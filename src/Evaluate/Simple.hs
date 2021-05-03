{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecordWildCards #-}

module Evaluate.Simple
    ( evaluateDArray
    , evaluate
    ) where

import           Control.Applicative    ((<|>))
import           Data.Bifunctor         (first, second, bimap)
import qualified Data.Map.Strict as Map
import           Data.Maybe             (isJust)
import qualified Data.Set        as Set

import           Debug.Trace

import           Common
import           Types

----------------------------------------------------------------------------------------------------

evaluateDArray :: Exp -> DArray
evaluateDArray exp =
  let term = makeLabel exp
   in toDArray . shareVars . reduceAndUpd term . initPreDArray term $ Map.empty


toDArray :: PreDArray -> DArray
toDArray = Map.map go
  where
    go :: Set.Set (Either Node Term) -> Set.Set Term
    go = Set.foldr (\nodeTerm acc -> either (const acc) (\x -> Set.insert x acc) nodeTerm) Set.empty


shareVars :: PreDArray -> PreDArray
shareVars predarr = if predarr == updPredarr then predarr else shareVars updPredarr
  where
    updPredarr = Map.foldrWithKey handleData Map.empty predarr

    handleData :: Node -> Set.Set (Either Node Term) -> PreDArray -> PreDArray
    handleData node set predarr' = if null updSet then predarr' else Map.insert node updSet predarr'
      where
        updSet = Set.foldr handleValue Set.empty set

        handleValue :: Either Node Term -> Set.Set (Either Node Term) -> Set.Set (Either Node Term)
        handleValue (Left node) set = maybe set (Set.union set) $ Map.lookup node predarr
        handleValue rightTerm   set = Set.insert rightTerm set


reduceAndUpd :: Term -> PreDArray -> PreDArray
reduceAndUpd term predarr =
  let (maybeTerm, updPredarr) = reduceOnceAndUpd term predarr
   in maybe updPredarr (flip reduceAndUpd updPredarr) maybeTerm
  where
    reduceOnceAndUpd :: Term -> PreDArray -> (Maybe Term, PreDArray)
    reduceOnceAndUpd (App lab (Lam _ (Abst x t)) rterm) predarr =
      (Just $ subst x rterm t,
         insertNode (R x)   (Left . C . label $ rterm)
       . insertNode (C lab) (Left . C . label $ t)     $ predarr)

    reduceOnceAndUpd term@(App {..})                    predarr =
      let lres@(lmaybeTerm, _) = reduceOnceAndUpd lterm predarr
          rres@(rmaybeTerm, _) = reduceOnceAndUpd rterm predarr
       in if (isJust lmaybeTerm)
            then first (fmap (\t -> term {lterm = t})) lres
            else first (fmap (\t -> term {rterm = t})) rres

    reduceOnceAndUpd term@(Lam lab (Abst x t))          predarr =
      first (fmap (Lam lab . Abst x)) $ reduceOnceAndUpd t predarr

    reduceOnceAndUpd _                                  predarr = (Nothing, predarr)


initPreDArray :: Term -> PreDArray -> PreDArray
initPreDArray (Var {..})                = insertNode (C label) (Left $ R name)
initPreDArray term@(Lam lab (Abst _ t)) = insertNode (C lab) (Right term) . initPreDArray t
initPreDArray (App {..})                = initPreDArray lterm . initPreDArray rterm


insertNode :: (Ord a) => Node -> a -> Map.Map Node (Set.Set a) -> Map.Map Node (Set.Set a)
insertNode node value map =
  let dataForNode    = Map.lookup node map
      updDataForNode = maybe (Set.singleton value) (Set.insert value) dataForNode
   in Map.insert node updDataForNode map

----------------------------------------------------------------------------------------------------

evaluate :: Term -> Term
evaluate term = maybe term evaluate (reduceOnce term)

reduceOnce :: Term -> Maybe Term
reduceOnce (App _ (Lam _ (Abst x t)) s) = Just $ subst x s t
reduceOnce (App {..})                   =
      (\t -> App label t rterm) <$> reduceOnce lterm
  <|> (\t -> App label lterm t) <$> reduceOnce rterm
reduceOnce (Lam lab (Abst x t))         = (Lam lab . Abst x) <$> reduceOnce t
reduceOnce _                            = Nothing

subst :: Name -> Term -> Term -> Term 
subst x s term@(Var {..})             = if x == name then s else term
subst x s (App {..})                  = App label (subst x s lterm) (subst x s rterm)
subst x s term@(Lam label (Abst y t)) = if x /= y then Lam label . Abst y $ subst x s t else term

----------------------------------------------------------------------------------------------------

