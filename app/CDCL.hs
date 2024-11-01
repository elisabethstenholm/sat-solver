module CDCL where

import Control.Applicative (Alternative (..))
import Control.Applicative.Logic
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import Prelude hiding (any)

-- A literal is either a variable or its negation
data Literal a
  = Var a
  | Neg a
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
data Antecedent a
  = Implied (Clause a)
  | Decision

-- Assignment of a variable to a bool and an antecedent
type Assignment a = Map a (Bool, Antecedent a)

-- The decision sequence is the list of assignments made during the search
type DecisionSeq a = [Assignment a]

-- Conclusion of the algorithm; either gives an assignment of values satisfying
-- the formula, or concludes that the formula is unsatisfiable
data Conclusion a
  = Satisfiable a
  | Unsatisfiable
  deriving (Show)

instance Functor Conclusion where
  fmap f (Satisfiable x) = Satisfiable (f x)
  fmap _ Unsatisfiable = Unsatisfiable

instance Applicative Conclusion where
  pure = Satisfiable
  Satisfiable f <*> x = f <$> x
  Unsatisfiable <*> _ = Unsatisfiable

instance Monad Conclusion where
  Satisfiable x >>= f = f x
  Unsatisfiable >>= _ = Unsatisfiable

instance Alternative Conclusion where
  empty = Unsatisfiable
  Unsatisfiable <|> x = x
  x <|> _ = x

-- Extract the set of variables from a formula
variables :: (Ord a) => Formula a -> Set a
variables = Set.unions . Set.map (Set.map litToVar)

-- Condition a formula on a set of literals
condition :: (Ord a) => Formula a -> Set (Literal a) -> Formula a
condition formula lits =
  Set.map (`Set.difference` Set.map neg lits) $
    Set.filter (Set.disjoint lits) formula

-- Unit resolution
unitResolution :: (Ord a) => Formula a -> (Set (Literal a), Formula a)
unitResolution formula =
  let (lits, nonUnitClauses) =
        first Set.unions $ Set.partition ((== 1) . Set.size) formula
   in if null lits
        then (Set.empty, formula)
        else
          let (lits', formula') =
                unitResolution $ condition nonUnitClauses lits
           in (Set.union lits lits', formula')

chooseLit :: Set a -> Maybe (Literal a)
chooseLit = fmap Var . convert

dpll :: (Ord a) => Formula a -> Conclusion (Set (Literal a))
dpll formula = do
  let (lits, formula') = unitResolution formula
  guard $ Set.empty `Set.notMember` formula'
  case chooseLit (variables formula') of
    Nothing -> Satisfiable lits
    Just l ->
      dpllAssuming formula' lits l
        <|> dpllAssuming formula' lits (neg l)
  where
    dpllAssuming formula' lits l = do
      lits' <- dpll (condition formula' (Set.singleton l))
      Satisfiable $ Set.unions [lits, lits', Set.singleton l]
