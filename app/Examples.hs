module Examples where

import qualified Data.Set as Set
import Types

-- A ∨ ¬A
-- Satisfiable {A ↦ True}
f1 :: Formula Char
f1 = Set.fromList [Set.fromList [Var 'A', Neg 'A']]

-- A ∧ ¬A
-- Unsatisfiable
f2 :: Formula Char
f2 = Set.fromList [Set.fromList [Var 'A'], Set.fromList [Neg 'A']]

-- (¬A ∨ B) ∧ (¬B ∨ ¬C) ∧ (C ∨ ¬D)
-- Satisfiable {A ↦ True, B ↦ True, C ↦ False, D ↦ False}
f3 :: Formula Char
f3 =
  Set.fromList
    [ Set.fromList [Neg 'A', Var 'B'],
      Set.fromList [Neg 'B', Neg 'C'],
      Set.fromList [Var 'C', Neg 'D']
    ]

-- (C ∨ A ∨ ¬B) ∧ (¬C ∨ A ∨ ¬B) ∧ (A ∨ B) ∧ ¬A
-- Unsatisfiable
f4 :: Formula Char
f4 =
  Set.fromList
    [ Set.fromList [Var 'C', Var 'A', Neg 'B'],
      Set.fromList [Neg 'C', Var 'A', Neg 'B'],
      Set.fromList [Var 'A', Var 'B'],
      Set.fromList [Neg 'A']
    ]
