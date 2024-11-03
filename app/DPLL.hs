{-# LANGUAGE TupleSections #-}

module DPLL where

import Control.Applicative
import Control.Applicative.Logic
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import Types

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

-- Assign a boolean to a variable
assignVar :: Set a -> Maybe (a, Bool)
assignVar = fmap (,True) . convert

assignBools :: (Ord a) => Set (Literal a) -> Map a Bool
assignBools = Map.fromAscList . Set.toAscList . Set.map assignBool

-- The DPLL algorithm
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
