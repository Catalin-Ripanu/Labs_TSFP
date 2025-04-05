module Automata where

import Control.Monad

type Q = Int
type Σ = Char

data DFA q σ = DFA
    { initial :: q
    , transition :: q -> σ -> q
    , isFinal :: q -> Bool
    }

-- https://en.wikipedia.org/wiki/Deterministic_finite_automaton#Example
evenZeros :: DFA Q Σ
evenZeros = DFA 1 transition (== 1)
  where
    transition q σ = case (q, σ) of
        (1, '0') -> 2
        (1, '1') -> 1
        (2, '0') -> 1
        (2, '1') -> 2

{-
    *** TODO 1 ***
    runDFA simulates a DFA on an input string:
    - Start from the initial state
    - For each input symbol, apply the transition function
    - Return the final state reached
    
    Example: 
    runDFA evenZeros "0110" = 1 (even zeros)
    runDFA evenZeros "010" = 2  (odd zeros)
-}
runDFA :: DFA q σ -> [σ] -> q
runDFA (DFA init trans _) = foldl trans init

{-
    acceptsDFA uses runDFA to check if a string is accepted:
    - Run the DFA on the input
    - Check if the final state is accepting
-}
acceptsDFA :: DFA q σ -> [σ] -> Bool
acceptsDFA dfa = isFinal dfa . runDFA dfa

data NFA q σ = NFA
    { initial' :: q
    , transition' :: q -> σ -> [q]
    , isFinal' :: q -> Bool
    }

-- https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton#Example
-- p = 1, q = 2
endsWithOne :: NFA Q Σ
endsWithOne = NFA 1 transition (== 2)
  where
    transition q σ = case (q, σ) of
        (1, '0') -> [1]
        (1, '1') -> [1, 2]
        _        -> []  -- missing transitions

{-
    *** TODO 2 ***
    runNFA simulates an NFA on an input string:
    - Start from the initial state
    - For each input symbol:
        * Apply the transition function to all current states
        * Collect all possible next states
    - Return all possible final states
    
    Difference from runDFA:
    - Instead of one state, we track multiple possible states
    - Use concatMap to handle the non-deterministic transitions
    
    Example:
    runNFA endsWithOne "01" = [1,2] (can end in either state)
    runNFA endsWithOne "00" = [1]   (can only end in state 1)
-}
runNFA :: NFA q σ -> [σ] -> [q]
runNFA (NFA init trans _) = foldl (\qs σ -> concatMap (`trans` σ) qs) [init]
-- Monadic Functional ?

{-
    acceptsNFA checks if any possible final state is accepting:
    - Run the NFA to get all possible final states
    - Check if any of them is accepting (using any)
-}
acceptsNFA :: NFA q σ -> [σ] -> Bool
acceptsNFA nfa = any (isFinal' nfa) . runNFA nfa
