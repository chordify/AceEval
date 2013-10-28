
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


module ACE.Evaluation (
    module ACE.Evaluation.EqIgnore
  , module ACE.Evaluation.ChordEq
  , module ACE.Evaluation.Func
  , module ACE.Evaluation.Segmentation
  ) where

import ACE.Evaluation.EqIgnore
import ACE.Evaluation.ChordEq
import ACE.Evaluation.Func
import ACE.Evaluation.Segmentation
