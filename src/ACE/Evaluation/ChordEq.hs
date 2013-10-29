
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


module ACE.Evaluation.ChordEq (
    -- * Chord and key equality functions
      rootOnlyEq
    , bassOnlyEq
    , majMinEq
    , triadEq
    , mirex2010
    , chordClassEq
    -- * Ground-truth wrapping
    , RefLab
    , refLab
    , makeGT
  ) where

import ACE.Evaluation.EqIgnore
import ACE.Evaluation.ChordClass
  
import HarmTrace.Base.Time 
import HarmTrace.Base.Chord 

import Data.IntSet               ( IntSet, size, intersection, member )

--------------------------------------------------------------------------------
-- Chord and key equality functions
--------------------------------------------------------------------------------

-- We wrap the groundtruth reference 'ChordLabel' into a new type to make sure
-- it is different from the machine annotated 'ChordLabel' at the type level
newtype RefLab = RefLab {refLab :: ChordLabel} deriving (Eq)

instance Show RefLab where
  show (RefLab c) = "Ref: " ++ show c

makeGT :: [Timed ChordLabel] -> [Timed RefLab]
makeGT = map (fmap RefLab)

-- unWrapGT :: [Timed RefLab] -> [Timed ChordLabel] 
-- unWrapGT = map (fmap refLab)

rootOnlyEq :: RefLab -> ChordLabel -> EqIgnore
rootOnlyEq gt test = chordCompare rootEq (\_ _ -> Equal) gt test -- Ignore all but root

bassOnlyEq :: RefLab -> ChordLabel -> EqIgnore
bassOnlyEq gt test = chordCompare (\_ _ -> Equal) bassEq gt test where

  bassEq :: RefLab -> ChordLabel -> EqIgnore
  bassEq (RefLab gt') test' = bassPC gt' ==* bassPC test'

-- | enharmonic equality for 'Root' 'Note's, N == N, X == X, and G# == Ab
rootEq :: Root -> Root -> EqIgnore
rootEq a b = toPitchClass a ==* toPitchClass b

-- | Compares a ground-truth 'ChordLabel' (first argument) to a test 
-- 'ChordLabel' (second argument) and returns True if the 'Root's of 
-- the chord's are equal, and both chords are major or minor
--
-- N.B. This equality function is non-associative because comparing a 
-- non-triadic chord is ignored in the evaluation while a non-triadic
-- chord in a evaluated annotation should just be qualified not equal
-- (because if also machine annotated non-triadic chords would be ignored
-- automatic transcription evaluation could be biased by outputting 
-- non-triadic chords at uncertain positions). 
-- 
-- >>> majMinEq (RefLab $ Chord (Note Nothing A) Sus4 [] 0 0) (Chord (Note Nothing A) Maj [] 0 0)
-- >>> Ignore
--
-- >>> majMinEq (RefLab $ Chord (Note Nothing A) Maj [] 0 0) (Chord (Note Nothing A) Min [] 0 0)
-- >>> NotEq
--
majMinEq :: RefLab -> ChordLabel -> EqIgnore
majMinEq gt test = chordCompare rootEq majMin gt test
   
  -- ignore the NoClass and only return True in case of maj/maj and min/min
  where majMin :: RefLab -> ChordLabel -> EqIgnore
        majMin x y = case ( toMajMin . toTriad $ refLab x
                          , toMajMin $ toTriad          y ) of
                       (MajClass, MajClass) -> Equal
                       (MajClass, MinClass) -> NotEq
                       (MinClass, MinClass) -> Equal
                       (MinClass, MajClass) -> NotEq
                       (NoClass , _       ) -> Ignore
                       (_       , NoClass ) -> Ignore
                       -- cannot happen
                       _ -> error "majMin: unexpected chord class"


-- | Returns True if both 'ChordLabel's are equal at the triad level: they are
-- either moth major or both minor. "None Chords" match only with other "None
-- Chords" and with nothing else
triadEq :: RefLab -> ChordLabel -> EqIgnore
triadEq gt test = chordCompare rootEq triadEqI gt test where

  triadEqI :: RefLab -> ChordLabel -> EqIgnore
  triadEqI (RefLab x) y = case (toTriad x, toTriad y) of
     (NoTriad, _      ) -> susEq x y
     (_      , NoTriad) -> NotEq
     (tx     , ty     ) -> tx ==* ty 
                   
  susEq :: ChordLabel -> ChordLabel -> EqIgnore
  susEq a b | isSus2 a     = toEqIgnore (isSus2 b)
            | isSus4 a     = toEqIgnore (isSus4 b)
            | otherwise    = Ignore
            
  
chordClassEq :: RefLab -> ChordLabel -> CCEval EqIgnore
chordClassEq rfl test = case (refLab rfl, test) of
   (NoChord,    NoChord   ) -> toCCEval Equal
   (UndefChord, _         ) -> toCCEval Ignore
   (_         , UndefChord) -> toCCEval NotEq
   (_         , NoChord   ) -> toCCEval NotEq
   (NoChord   , _         ) -> toCCEval NotEq
   (gt        , _         ) -> compareCC (toChordClass gt) (toChordClass test)

  
-- compares the 'NoChord' and 'UndefChord' cases, such that this does not have
-- to be replicated in all Eq's
chordCompare :: (Root -> Root -> EqIgnore) 
             -> (RefLab -> ChordLabel -> EqIgnore) 
             ->  RefLab -> ChordLabel -> EqIgnore
chordCompare rEq cEq rf t = case (refLab rf, t) of
                             (NoChord,    NoChord   ) -> Equal
                             (UndefChord, _         ) -> Ignore
                             (_         , UndefChord) -> NotEq
                             (_         , NoChord   ) -> NotEq
                             (NoChord   , _         ) -> NotEq
                             (gt, _) ->   chordRoot gt `rEq` chordRoot t
                                     &&*            rf `cEq`           t
                 
                 
mirex2010 :: RefLab -> ChordLabel -> EqIgnore
mirex2010 (RefLab NoChord)    NoChord    = Equal
mirex2010 (RefLab UndefChord) UndefChord = Equal -- Ignore
mirex2010 (RefLab UndefChord) _          = NotEq -- Ignore
mirex2010 _                   UndefChord = NotEq
mirex2010 (RefLab NoChord)    _          = NotEq
mirex2010 _                   NoChord    = NotEq
mirex2010 (RefLab gt)         test       = 
  let bassMatch :: IntSet -> ChordLabel -> Int
      bassMatch _ (Chord _ _ _ (Note Nat I1)) = 0
      bassMatch is c | bassPC c `member` is   = 1
                     | otherwise              = 0
                   
      gtpc  = pc . toPitchClasses $ gt
      tstpc = pc . toPitchClasses $ test
      
      i     = gtpc `intersection` tstpc
      p     = size i
      p' = p + bassMatch gtpc test + bassMatch tstpc gt
      
  in case (chordShorthand gt, chordShorthand test) of
      (Aug, _  ) -> p' >=* 2
      (Dim, _  ) -> p' >=* 2
      _          -> p' >=* 3
