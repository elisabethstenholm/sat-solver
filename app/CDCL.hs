module CDCL where

import Control.Applicative
import Control.Applicative.Logic
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import Types
import Prelude hiding (any)

-- The antecedent of a variable assignment is either a decision
-- or a clause that implies it through unit resolution
data Antecedent a
  = Implied (Clause a)
  | Decision

-- Assignment of a variable to a bool and an antecedent
type Assignment a = Map a (Bool, Antecedent a)

-- The decision sequence is the list of assignments made during the search
type DecisionSeq a = [Assignment a]
