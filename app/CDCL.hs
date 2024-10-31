module CDCL where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

-- A literal is either a variable or its negation
data Literal a = Var a | Neg a
  deriving (Eq, Ord)

-- A clause is a list of literals; [a,b,c] corresponds to (a ∨ b ∨ c)
type Clause a = [Literal a]
-- A formula is a list of clauses: [x,y,z] corresponds to (x ∧ y ∧ z)
type Formula a = [Clause a]

-- The antecedent of a variable assignment is either a decision
-- or a clause that implies it through unit resolution
data Antecedent a = Cl (Clause a) | Decision
-- The tree level at which the variable assignment was made
type DecisionLevel = Integer

-- Assignment of values to variables over the course of the search
type Assignment a = Map a (Bool, Antecedent a, DecisionLevel)


