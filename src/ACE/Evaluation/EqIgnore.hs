
--------------------------------------------------------------------------------
-- |
-- Module      :  Evaluation
-- Copyright   :  (c) 2010-2013 Chordify, Universiteit Utrecht, University of Oxford
-- License     :  LGPL3
--
-- Maintainer  :  bash@cs.uu.nl, jpm@cs.ox.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: A module for evaluating chord and key annotations
--------------------------------------------------------------------------------


module ACE.Evaluation.EqIgnore (
    -- * Small Evaluation Logic
      EqIgnore (..)
    , (==*)
    , (>=*)
    , (<=*)
    , (&&*)
    , (||*)
    , ignore
    , equal
    , toEqIgnore
  ) where

--------------------------------------------------------------------------------
-- A very small logic for comparing results including a wildcard (Ignore)
--------------------------------------------------------------------------------

-- A datatype for comparisons that can be equal, not equal, or ignored  
data EqIgnore = Equal   -- ^ Equal
              | NotEq   -- ^ Not equal
              | Ignore  -- ^ Ignored
                deriving (Eq)
                
instance Show EqIgnore where
  show Equal  = "==*"
  show NotEq  = "/=*"
  show Ignore = "***"

-- | Behaves like 'Eq' but returns an 'EqIgnore' ('Equal' or 'NotEq').
(==*) :: Eq a => a -> a -> EqIgnore
a ==* b = compEqIgnore (==) a b

infix 4 ==* -- same "infixity" as (==)

-- | Behaves like 'Eq' but returns an 'EqIgnore' ('Equal' or 'NotEq').
(>=*) :: Ord a => a -> a -> EqIgnore
a >=* b = compEqIgnore (>=) a b
        
infix 4 >=* -- same "infixity" as (>=)

-- | Behaves like 'Eq' but returns an 'EqIgnore' ('Equal' or 'NotEq').
(<=*) :: Ord a => a -> a -> EqIgnore
a <=* b = compEqIgnore (<=) a b
        
infix 4 <=* -- same "infixity" as (>=)

compEqIgnore :: (a -> a -> Bool) -> a -> a -> EqIgnore
compEqIgnore eq a b | a `eq` b  = Equal
                    | otherwise = NotEq

-- | Behaves like a regular conjunction '(&&)', but when comparing a 'Ignore'
-- will return an 'Ignore' again.
(&&*) :: EqIgnore -> EqIgnore -> EqIgnore
Ignore &&* _      = Ignore
_      &&* Ignore = Ignore
Equal  &&* Equal  = Equal
_      &&* _      = NotEq

infix 3 &&* -- same "infixity" as (&&)

-- | Behaves like a regular conjunction '(&&)', but when comparing a 'Ignore'
-- will return an 'Ignore' again.
(||*) :: EqIgnore -> EqIgnore -> EqIgnore
Ignore ||* _      = Ignore
_      ||* Ignore = Ignore
NotEq  ||* NotEq  = NotEq
_      ||* _      = Equal

infix 3 ||* -- same "infixity" as (||)


-- | Returns 'True' if the 'EqIgnore' is 'Ignore'.
ignore :: EqIgnore -> Bool
ignore Ignore = True
ignore _      = False

-- | Returns 'True' if the 'EqIgnore' is 'Equal'.
equal  :: EqIgnore -> Bool
equal  Equal  = True
equal  _      = False

-- | Returns 'Equal' if 'True' and 'NotEq' otherwise.
toEqIgnore :: Bool -> EqIgnore
toEqIgnore True = Equal
toEqIgnore _    = NotEq
