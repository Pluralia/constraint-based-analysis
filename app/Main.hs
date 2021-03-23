module Main where

import           Data.Bifunctor         (first, second)
import qualified Data.Map.Strict as Map
import           Data.Maybe             (maybe)
import qualified Data.Set        as Set
import           Debug.Trace
import           Text.Read


----------------------------------------------------------------------------------------------------
--types

type Name = String
type Lab  = Integer

infixl 4 :@:
data Exp = V Name
         | Exp :@: Exp
         | L Name Exp
  deriving (Eq, Read, Show)

data Abst = Abst Name Term
  deriving (Eq, Read, Show, Ord)

data Term = Var Lab Name
          | App Lab Term Term
          | Lam Lab Abst
  deriving (Eq, Read, Show, Ord)

data Node = C Lab
          | R Name
  deriving (Eq, Read, Show, Ord)

type Rhs = Node

data Lhs = N Node | A Abst
  deriving (Eq, Read, Show)

data Constraint = Simple Lhs Rhs
                | Complex Abst Rhs Lhs Rhs
  deriving (Eq, Read, Show)


type Worklist = [Node]
type DArray = Map.Map Node (Set.Set Term)
type Edges = Map.Map Node [Constraint]

type CBA = (Map.Map Lab (Set.Set Term), Map.Map Name (Set.Set Term))


----------------------------------------------------------------------------------------------------
--examples

twoIdApp :: Exp
twoIdApp = (L "x" $ V "x") :@: (L "y" $ V "y")

fghOp :: Exp
fghOp = (L "f" $ L "g" $ L "h" $ (V "f" :@: V "g") :@: (V "g" :@: V "h"))
  :@: (L "x" $ V "x" :@: V "a")
  :@: (L "y" $ V "y" :@: V "b")
  :@: (L "z" $ V "z" :@: V "c")


----------------------------------------------------------------------------------------------------
--library

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


getLabel :: Term -> Lab
getLabel (Var label _)   = label
getLabel (App label _ _) = label
getLabel (Lam label _)   = label


makeConstraints :: Term -> [Constraint]
makeConstraints = fst . go [] []
  where
    go :: [Constraint] -> [Abst] -> Term -> ([Constraint], [Abst])
    go acc absts (Var label name)        = (Simple (N $ R name) (C label) : acc, absts)
    
    go acc absts x@(App label lterm rterm) =
      trace (show x ++ ":   " ++ show updAbsts ++ "\n") $ (foldr handleAbst updAcc updAbsts, updAbsts)
      where
        (lacc, labsts)   = go acc absts lterm
        (updAcc, rabsts) = go lacc absts rterm
        updAbsts         = labsts ++ rabsts

        llabel, rlabel :: Lab
        llabel = getLabel lterm
        rlabel = getLabel rterm

        handleAbst :: Abst -> [Constraint] -> [Constraint]
        handleAbst abst@(Abst name term) acc =       
            Complex abst (C llabel) (N $ C rlabel)          (R name)
          : Complex abst (C llabel) (N . C $ getLabel term) (C label)
          : acc 
    
    go acc absts (Lam label abst)        = (Simple (A abst) (C label) : handleAbst abst, abst : absts)
      where
        handleAbst :: Abst -> [Constraint]
        handleAbst (Abst name term) = fst $ go acc absts term


resolveConstraints :: [Constraint] -> CBA 
resolveConstraints constraints =
  let graph  = buildGraph constraints $ (([], Map.empty), Map.empty)
      darray = trace (show graph ++ "\n\n------------------------------------------------\n\n") $ iteration graph
   in trace (show darray ++ "\n\n------------------------------------------------\n\n") $ recordSolution darray
  where
    -- auxiliary
    add :: Node -> Term -> (Worklist, DArray) -> (Worklist, DArray)
    add q d worklistDArray@(worklist, darray) =
      let dataForQ = Map.lookup q darray
       in if (maybe False (Set.member d) dataForQ)
            then worklistDArray
            else (q : worklist, Map.insert q (maybe (Set.singleton d) (Set.insert d) dataForQ) darray)

    addByNode :: Node -> Node -> (Worklist, DArray) -> (Worklist, DArray)
    addByNode node sourceNode worklistDArray@(_, darray) =
      let dataForNode = Map.lookup sourceNode darray
       in Set.foldr (add node) worklistDArray $ maybe Set.empty id dataForNode

    addEdge :: Node -> [Constraint] -> Edges -> Edges
    addEdge = Map.insertWith (\[cc] -> (cc :))


    -- 1st [Initialization] + 2nd [Build the graph] steps
    buildGraph :: [Constraint] -> ((Worklist, DArray), Edges) -> ((Worklist, DArray), Edges)
    buildGraph []                                      res                     = res
    buildGraph (cc@(Simple (A t) p)        : constraints) (worklistDArray, edges) =
      trace ("S A: " ++ show cc ++ "\n") $ buildGraph constraints (add p (Lam 0 t) worklistDArray, edges)
    buildGraph (cc@(Simple (N p1) p2)      : constraints) (worklistDArray, edges) =
      trace ("S N: " ++ show cc ++ "\n") $ buildGraph constraints (worklistDArray, addEdge p1 [cc] edges)
    buildGraph (cc@(Complex t p (N p1) p2) : constraints) (worklistDArray, edges) =
      trace ("C  : " ++ show cc ++ "\n") $ buildGraph constraints (worklistDArray, addEdge p [cc] $ addEdge p1 [cc] edges)


    -- 3rd [Iteration] step
    iteration :: ((Worklist, DArray), Edges) -> DArray
    iteration (([], darray), _)                      = darray
    iteration ((w@(workNode : worklist), darray), edges) =
      trace (show w ++ ": " ++ show darray ++ "\n\n=======\n") $ 
      iteration $ (go (maybe [] id $ Map.lookup workNode edges) (worklist, darray), edges)
      where
        go :: [Constraint] -> (Worklist, DArray) -> (Worklist, DArray)
        go []                                        res                        = res
        go (cc@(Simple (A p1) p2)      : constraint) worklistDArray@(_, darray) =
          error "iteration.go: abstraction as an argument is impossible"
        go (cc@(Simple (N p1) p2)      : constraint) worklistDArray             = go constraint (addByNode p2 p1 worklistDArray)
        go (cc@(Complex t p (N p1) p2) : constraint) worklistDArray@(_, darray) =
          if (maybe False (Set.member (Lam 0 t)) $ Map.lookup p darray)
            then go constraint $ addByNode p2 p1 worklistDArray
            else go constraint worklistDArray


    -- 4th [Recording the solution] step
    recordSolution :: DArray -> CBA
    recordSolution = Map.foldrWithKey go (Map.empty, Map.empty)
      where
        go :: Node -> Set.Set Term -> CBA -> CBA
        go (C lab)  terms = first (Map.insert lab terms)
        go (R name) terms = second (Map.insert name terms)


----------------------------------------------------------------------------------------------------
--main

main :: IO ()
main = do
  input <- getLine
  let exp   = maybe twoIdApp id (readMaybe input :: Maybe Exp)
--  let exp   = maybe fghOp id (readMaybe input :: Maybe Exp)
      term  = makeLabel exp
  print $ resolveConstraints . reverse . makeConstraints $ term

----------------------------------------------------------------------------------------------------


