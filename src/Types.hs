module Types where

import qualified Data.Map.Strict as Map
import           Data.List              (intercalate)
import qualified Data.Set        as Set

----------------------------------------------------------------------------------------------------

type Name = String
type Lab  = Integer


infixl 4 :@:
data Exp = V Name
         | Exp :@: Exp
         | L Name Exp
  deriving (Eq, Read, Show)


-- extract abstraction to add recursive lambdas
data Abst = Abst Name Term
  deriving (Eq, Read, Ord)

data Term = Var { label :: Lab, name :: Name }
          | App { label :: Lab, lterm :: Term, rterm :: Term }
          | Lam { label :: Lab, abst :: Abst }
  deriving (Eq, Read, Ord)


data Node = C Lab
          | R Name
  deriving (Eq, Read, Ord)

type Rhs = Node

data Lhs = N Node
         | A Lab Abst
  deriving (Eq, Read)


data Constraint = Simple Lhs Rhs
                | Complex (Lab, Abst) Rhs Lhs Rhs
  deriving (Eq, Read)


type Worklist = [Node]

type DArray   = Map.Map Node (Set.Set Term)
type PreDArray = Map.Map Node (Set.Set (Either Node Term))

type Edges    = Map.Map Node [Constraint]

type CBA = (Map.Map Lab (Set.Set Term), Map.Map Name (Set.Set Term))

----------------------------------------------------------------------------------------------------

labSymb, inSymb :: String
labSymb = "."
inSymb = " :: "

instance Show Abst where
  show (Abst name term) = "{ fn " ++ name ++ " -> " ++ show term ++ " }"

instance Show Term where
  show (Var label name)        = name ++ labSymb ++ show label
  show (App label lterm rterm) =
    "( " ++ show lterm ++ " " ++ show rterm ++ " )" ++ labSymb ++ show label
  show (Lam label abst)        = show abst ++ labSymb ++ show label

instance Show Node where
  show (C label) = "C(" ++ show label ++ ")"
  show (R name)  = "r(" ++ name ++ ")"

instance Show Lhs where
  show (N node)       = show node
  show (A label abst) = show abst ++ labSymb ++ show label 

instance Show Constraint where
  show (Simple l r)           = show l ++ inSymb ++ show r
  show (Complex abst r1 l r2) =
    show abst ++ inSymb ++ show r1 ++ " => " ++ show l ++ inSymb ++ show r2

showDArray :: DArray -> String
showDArray =
    ("\n       " ++)
  . intercalate "\n       "
  . fmap (\(node, termsSet) -> show node ++ "  --->  " ++ (show . Set.toList $ termsSet))
  . Map.toList

showEdges :: Edges -> String
showEdges =
    ("\n  " ++)
  . intercalate "\n  "
  . fmap (\(node, constraints) ->
     show node ++ " ---> \n            " ++ (intercalate "\n            " . fmap show $ constraints))
  . Map.toList

----------------------------------------------------------------------------------------------------
