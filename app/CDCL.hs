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

-- Enumerated clauses
type NumberedClauses a = Map Integer (Clause a)

-- Enumerate the clauses in a formula
numberClauses :: Formula a -> NumberedClauses a
numberClauses = Map.fromAscList . zip [1 ..] . Set.toAscList

-- Maps a literal to the set of (identifiers of) clauses it is watched by
type Watched a = Map (Literal a) (Set Integer)

-- Check if a given literal can be assigned as watched
canBeWatched ::
  (Ord a) =>
  Watched a ->
  Map a Bool ->
  Integer ->
  Literal a ->
  Conclusion (Maybe (Literal a))
canBeWatched w varAss n l = do
  let isWatched = maybe False (n `Set.member`) (Map.lookup l w)
  let maybeVal = eval l varAss
  case (isWatched, maybeVal) of
    (_, Just True) -> return Nothing
    (True, Nothing) -> return Nothing
    (False, Nothing) -> return (Just l)
    (False, Just False) -> return Nothing
    (True, Just False) -> Unsatisfiable

-- Find a new literal in the clause to watch,
-- there may be none, in which case it is a unit clause,
-- or the clause is discovered to be falsified
findNewLiteral ::
  (Ord a) =>
  Watched a ->
  Map a Bool ->
  Integer ->
  Clause a ->
  Conclusion (Maybe (Literal a))
findNewLiteral w varAss n c = do
  maybeLits <- sequence <$> mapM (canBeWatched w varAss n) (Set.toList c)
  return (maybeLits >>= listToMaybe)

{-
Go through the set of literals in the clause and for each:
- check if evaluated to True, in which case do nothing
- check if already watched
  - if evaluated to False, return Unsatisfied
  - otherwise, move on
- check if evaluated to False, in which case move on
Terminate if finding any unassigned and unwatched literal
If no such is found, declare unit clause
   -}

-- Given an assignment of a boolean to a variable, update the clauses
-- watching the falsified literal and return literals of unit clauses
updateWatchers ::
  (Ord a) =>
  NumberedClauses a ->
  Watched a ->
  Map a Bool ->
  Literal a ->
  Conclusion (Watched a, Set (Literal a))
updateWatchers clauseMap w varAss l =
  case Map.lookup l w of
    Nothing -> return (w, Set.empty)
    Just cs -> do
      let cs' = second (Set.delete l) <$> Map.toList (Map.restrictKeys clauseMap cs)
      foldr combine (return (w, Set.empty)) cs'
      where
        combine (n, c) x = do
          (w', units) <- x
          maybeLit <- findNewLiteral w' varAss n c
          case maybeLit of
            Nothing -> return (w', _)
            Just l' -> _

-- The antecedent of a variable assignment is either a decision
-- or a clause that implies it through unit resolution
data Antecedent a
  = Implied (Clause a)
  | Decision

-- Assignment of a variable to a bool and an antecedent
type Assignment a = Map a (Bool, Antecedent a)

-- The decision sequence is the list of assignments made during the search
type DecisionSeq a = [Assignment a]
