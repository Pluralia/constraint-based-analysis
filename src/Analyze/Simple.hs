{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecordWildCards #-}

module Analyze.Simple
    ( analyzeDArray
    , makeConstraints
    , resolveConstraints
    , toCBA
    ) where

import           Data.Bifunctor         (first, second)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Common
import           Types

import           Debug.Trace

----------------------------------------------------------------------------------------------------

analyzeDArray :: Exp -> DArray
analyzeDArray = resolveConstraints . makeConstraints . makeLabel

----------------------------------------------------------------------------------------------------

makeConstraints :: Term -> [Constraint]
makeConstraints term = go term []
  where
    go :: Term -> [Constraint] -> [Constraint]
    go (Var {..})              acc = Simple (N $ R name) (C label) : acc
    
    go (Lam {..})              acc = Simple (A label abst) (C label) : handleAbst abst
      where
        handleAbst :: Abst -> [Constraint]
        handleAbst (Abst _ term) = go term acc

    go (App {label = lab, ..}) acc = foldr handleAbst (go rterm . go lterm $ acc) absts
      where
        handleAbst :: (Lab, Abst) -> [Constraint] -> [Constraint]
        handleAbst x@(_, (Abst name term)) acc =
            Complex x (C . label $ lterm) (N . C . label $ rterm) (R name)
          : Complex x (C . label $ lterm) (N . C . label $ term)  (C lab)
          : acc
    -------------------------------------------------------------------------
    absts :: [(Lab, Abst)]
    absts = getAbsts term []
      where
        getAbsts :: Term -> [(Lab, Abst)] -> [(Lab, Abst)]
        getAbsts (Var _ _)                   = id
        getAbsts (Lam label abst@(Abst _ t)) = ((label, abst) :) . (getAbsts t)
        getAbsts (App {..})                  = getAbsts lterm . getAbsts rterm

----------------------------------------------------------------------------------------------------

resolveConstraints :: [Constraint] -> DArray
resolveConstraints constraints =
--  trace ("Edges:" ++ showEdges edges ++ "\n\n--------------------------------------------\n\n") $
  darray
  where
    (worklistDarray, edges)  = buildGraph constraints (([], Map.empty), Map.empty)
    darray                   = iteration worklistDarray

    -- auxiliary functions
    add :: Node -> Term -> (Worklist, DArray) -> (Worklist, DArray)
    add q d worklistDArray@(worklist, darray) =
      let dataForQ = Map.lookup q darray
       in if (maybe False (Set.member d) dataForQ)
            then worklistDArray
            else (q : worklist, Map.insert q (maybe (Set.singleton d) (Set.insert d) dataForQ) darray)

    addEdge :: Node -> Constraint -> Edges -> Edges
    addEdge node = Map.insertWith (\[cc] -> (cc :)) node . (:[])

    addByNode :: Node -> Node -> (Worklist, DArray) -> (Worklist, DArray)
    addByNode node sourceNode worklistDArray@(_, darray) =
      let dataForSourceNode = Map.lookup sourceNode darray
       in Set.foldr (add node) worklistDArray $ maybe Set.empty id dataForSourceNode


    -- 1st [Initialization] + 2nd [Build the graph] steps
    buildGraph :: [Constraint] -> ((Worklist, DArray), Edges) -> ((Worklist, DArray), Edges)
    buildGraph []                                         res                     = res

    buildGraph (cc@(Simple (A l t) p)      : constraints) (worklistDArray, edges) =
      buildGraph constraints (add p (Lam l t) worklistDArray, edges)

    buildGraph (cc@(Simple (N p1) p2)      : constraints) (worklistDArray, edges) =
      buildGraph constraints (worklistDArray, addEdge p1 cc edges)

    buildGraph (cc@(Complex t p (N p1) p2) : constraints) (worklistDArray, edges) =
      buildGraph constraints (worklistDArray, addEdge p cc . addEdge p1 cc $ edges)


    -- 3rd [Iteration] step
    iteration :: (Worklist, DArray) -> DArray
    iteration ([], darray)                      = darray
    iteration (w@(worknode : worklist), darray) =
{-
      trace (show w ++
             ":" ++
             showDArray darray ++
             "\n\n---------------------------------------\n" ++
             show currentConstraints ++
             "\n\n=======================================\n") $ 
-}
      iteration $ handleConstraints currentConstraints (worklist, darray)
      where
        currentConstraints = (maybe [] id $ Map.lookup worknode edges)

        handleConstraints :: [Constraint] -> (Worklist, DArray) -> (Worklist, DArray)
        handleConstraints []                                                res            = res

        handleConstraints ((Simple (A _ _) _) : _)                          _              =
          error "iteration.go: a constraint for an abstraction is impossible here"

        handleConstraints ((Simple (N p1) p2) : constraints)                worklistDArray =
          handleConstraints constraints $ addByNode p2 p1 worklistDArray

        handleConstraints ((Complex (lab, abst) p (N p1) p2) : constraints) worklistDArray =
          handleConstraints constraints $
            if (maybe False (Set.member (Lam lab abst)) $ Map.lookup p darray)
              then addByNode p2 p1 worklistDArray
              else worklistDArray

----------------------------------------------------------------------------------------------------

-- 4th [Recording the solution] step
toCBA :: DArray -> CBA
toCBA = Map.foldrWithKey go (Map.empty, Map.empty)
  where
    go :: Node -> Set.Set Term -> CBA -> CBA
    go (C lab)  terms = first (Map.insert lab terms)
    go (R name) terms = second (Map.insert name terms)

----------------------------------------------------------------------------------------------------

