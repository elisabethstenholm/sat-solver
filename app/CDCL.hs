{-# LANGUAGE TupleSections #-}

module CDCL where

import Control.Applicative
import Control.Applicative.Logic
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import Types
import Prelude hiding (any)

type LabeledClauses label a = Map label (Clause a)

-- The antecedent of a variable assignment is either a decision
-- or a clause that implies it through unit resolution
data Antecedent label
  = Implied label
  | Decision

-- Assignment of a variable to a bool and an antecedent
type Assignment label a = Map a (Bool, Antecedent label)

-- The decision sequence is the list of assignments made during the search
type DecisionSeq label a = [Assignment label a]

-- Extract the set of variables from a formula
variables :: (Ord a) => Formula a -> Set a
variables = Set.unions . Set.map (Set.map litToVar)

-- Condition a formula on a literal
condition :: (Ord a) => Formula a -> Literal a -> Formula a
condition formula l =
  Set.map (Set.delete (neg l)) $ Set.filter (Set.notMember l) formula

conflictAnalysisAux :: Map a (Clause a) -> ([a], Set (Literal a)) -> ([a], Set (Literal a))
conflictAnalysisAux ([], lits) = ([], lits)
conflictAnalysisAux (x : xs, lits) = _

-- conflictAnalysis ::
--   (Ord label, Ord a) =>
--   LabeledClauses label a ->
--   DecisionSeq label a ->
--   label ->
--   Maybe (DecisionSeq label a, Clause a)
-- conflictAnalysis clauseMap ds l = do
--   c <- Map.lookup l clauseMap
--   let vc = Set.toList $ Set.map litToVar c
--   let foo = map _ vc
--   _

-- Unit resolution
-- unitResolution :: (Ord a) => Set label -> State (LabeledClauses label a) (Assignment label a)
-- unitResolution ls = do
--   clauseMap <- get
--   _

-- case convert (Map.filter ((== 1) . Set.size) $ Map.restrictKeys clauseMap ls) of
--   Nothing -> return (Map.empty, ls)
--   Just x -> _

{-   case convert (Set.unions $ Set.filter ((== 1) . Set.size) formula) of
    Nothing -> (Set.empty, formula)
    Just l ->
      let (lits', formula') =
            unitResolution $ condition formula l
       in (Set.insert l lits', formula') -}

-- Assign a boolean to a variable
-- assignVar :: Set a -> Maybe (a, Bool)
-- assignVar = fmap (,True) . convert

-- assignBools :: Set (Literal a) -> Assignment a
-- assignBools = _

-- cdcl :: Formula a -> StateT (DecisionSeq label a) Conclusion (Map a Bool)
-- cdcl formula = do
--   let (lits, formula') = unitResolution formula
--   guard $ Set.empty `Set.notMember` formula'
--   let ass = assignBools lits
--   case assignVar (variables formula') of
--     Nothing -> Satisfiable ass
--     Just x ->
--       dpllAssuming formula' ass x
--         <|> dpllAssuming formula' ass (second not x)
--   where
--     dpllAssuming formula' ass x = do
--       ass' <- dpll (condition formula' (assToLit x))
--       Satisfiable $ Map.unions [ass, ass', uncurry Map.singleton x]

{-
Unit resolution on the formula (updates assignments at current level)
Assign value to unassigned variable
Unit resolution
- If conflict found do conflict analysis, add to formula, then backtrack
 -}
