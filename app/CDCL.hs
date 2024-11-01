module CDCL where

import Control.Applicative.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import Prelude hiding (any)

-- A literal is either a variable or its negation
data Literal a = Var a | Neg a
  deriving (Eq, Ord)

instance (Show a) => Show (Literal a) where
  show (Var x) = show x
  show (Neg x) = '¬' : show x

instance Functor Literal where
  fmap f (Var x) = Var (f x)
  fmap f (Neg x) = Neg (f x)

litToVar :: Literal a -> a
litToVar (Var x) = x
litToVar (Neg x) = x

neg :: Literal a -> Literal a
neg (Var x) = Neg x
neg (Neg x) = Var x

-- A clause is a set of literals; {a,b,c} corresponds to (a ∨ b ∨ c)
type Clause a = Set (Literal a)

-- A formula is a set of clauses: {x,y,z} corresponds to (x ∧ y ∧ z)
type Formula a = Set (Clause a)

-- The antecedent of a variable assignment is either a decision
-- or a clause that implies it through unit resolution
data Antecedent a = Implied (Clause a) | Decision

-- Assignment of a variable to a bool and an antecedent
type Assignment a = Map a (Bool, Antecedent a)

-- The decision sequence is the list of assignments made during the search
type DecisionSeq a = [Assignment a]

-- Conclusion of the algorithm; either gives an assignment of values satisfying
-- the formula, or concludes that the formula is unsatisfiable
data Conclusion a = Satisfiable (Set (Literal a)) | Unsatisfiable

instance Functor Conclusion where
  fmap f (Satisfiable lits) = Satisfiable _

instance Applicative Conclusion where
  pure = _
  (<*>) = _

instance Monad Conclusion where
  x >>= f = _

-- Extract the set of variables from a formula
variables :: (Ord a) => Formula a -> Set a
variables = Set.unions . Set.map (Set.map litToVar)

-- Condition a formula on a set of literals
condition :: (Ord a) => Formula a -> Set (Literal a) -> Formula a
condition formula lits =
  Set.map (`Set.difference` Set.map neg lits) $ Set.filter (Set.disjoint lits) formula

-- Unit resolution
unitResolution :: (Ord a) => Formula a -> (Set (Literal a), Formula a)
unitResolution formula =
  let (lits, nonUnitClauses) = first Set.unions $ Set.partition ((== 1) . Set.size) formula
   in if null lits
        then (Set.empty, formula)
        else
          let (lits', formula') = unitResolution $ condition nonUnitClauses lits
           in (Set.union lits lits', formula')

dpll :: (Ord a) => Formula a -> Conclusion a
dpll formula =
  let (lits, formula') = unitResolution formula
   in case convert (Set.unions formula') of
        Nothing -> Satisfiable lits
        Just l ->
          if Set.empty `Set.member` formula'
            then Unsatisfiable
            else _

{- cdcl ::
  StateT
    (Formula a)
    (Reader (Formula a, Set a, Assignment a, CurrentLevel))
    (Conclusion a)
cdcl = do

  _ -}
