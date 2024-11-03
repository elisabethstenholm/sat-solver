{- Types used in the SAT solving algorithms -}

module Types where

import Control.Applicative (Alternative (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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

assToLit :: (a, Bool) -> Literal a
assToLit (x, True) = Var x
assToLit (x, False) = Neg x

assignBool :: Literal a -> (a, Bool)
assignBool (Var x) = (x, True)
assignBool (Neg x) = (x, False)

-- Evaluate a literal given an assignment to the variable
evalLit :: Literal a -> Bool -> Bool
evalLit (Var _) = id
evalLit (Neg _) = not

eval :: (Ord a) => Literal a -> Map a Bool -> Maybe Bool
eval x varAss = evalLit x <$> Map.lookup (litToVar x) varAss

-- A clause is a set of literals; {a,b,c} corresponds to (a ∨ b ∨ c)
type Clause a = Set (Literal a)

-- A formula is a set of clauses: {x,y,z} corresponds to (x ∧ y ∧ z)
type Formula a = Set (Clause a)

-- Conclusion of the SAT solving algorithm; either gives an assignment of values satisfying
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
