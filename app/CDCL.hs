{-# LANGUAGE TupleSections #-}

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

assignBool :: Literal a -> (a, Bool)
assignBool (Var x) = (x, True)
assignBool (Neg x) = (x, False)

assToLit :: (a, Bool) -> Literal a
assToLit (x, True) = Var x
assToLit (x, False) = Neg x

assignBools :: (Ord a) => Set (Literal a) -> Map a Bool
assignBools = Map.fromAscList . Set.toAscList . Set.map assignBool

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

-- Condition a formula on a literal
condition :: (Ord a) => Formula a -> Literal a -> Formula a
condition formula l =
  Set.map (Set.delete (neg l)) $ Set.filter (Set.notMember l) formula

-- Unit resolution
unitResolution :: (Ord a) => Formula a -> (Set (Literal a), Formula a)
unitResolution formula =
  case convert (Set.unions $ Set.filter ((== 1) . Set.size) formula) of
    Nothing -> (Set.empty, formula)
    Just l ->
      let (lits', formula') =
            unitResolution $ condition formula l
       in (Set.insert l lits', formula')

assignVar :: Set a -> Maybe (a, Bool)
assignVar = fmap (,True) . convert

dpll :: (Ord a) => Formula a -> Conclusion (Map a Bool)
dpll formula = do
  let (lits, formula') = unitResolution formula
  guard $ Set.empty `Set.notMember` formula'
  let ass = assignBools lits
  case assignVar (variables formula') of
    Nothing -> Satisfiable ass
    Just x ->
      dpllAssuming formula' ass x
        <|> dpllAssuming formula' ass (second not x)
  where
    dpllAssuming formula' ass x = do
      ass' <- dpll (condition formula' (assToLit x))
      Satisfiable $ Map.unions [ass, ass', uncurry Map.singleton x]
