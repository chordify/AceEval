
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


module Evaluation (
    -- * Evaluation functions
      -- relCorrectOverlap
      chordChangeRatio
    , avgDistToOne
    , overlapEval
    , overlapRatio
    , overlapRatioCol
    , weightOverlapRatio
    , reportAvgWOR
    -- * Ground-truth wrapping
    , RefLab 
    , refLab
    , makeGT
    -- * Chord and key equality functions
    , rootOnlyEq
    , majMinEq
    -- , triadEq
    , mirex2010
    -- * Small Evaluation Logic
    , EqIgnore
    , (==*)
    , (>=*)
    , (<=*)
    , (&&*)
    , (||*)
    , ignore
    , equal
    -- * Displaying evaluations 
    -- , printChordRCO
    -- , printRCO
    , printOverlapEval
    -- * Testing
    , totalDurationCheck
  ) where

import HarmTrace.Base.Time 
import HarmTrace.Base.Chord 

import Data.List                 ( genericLength, intercalate )
import Data.IntSet               ( IntSet, size, intersection, member )

import Data.Foldable             ( foldrM )
import Control.Monad.State       ( State, execState, modify )
import Text.Printf               ( printf )

--------------------------------------------------------------------------------
--  Constants
--------------------------------------------------------------------------------
{-
evaluationSampleRate, displaySampleRate :: NumData
-- | The sample rate used in a normal (non-visual) comparison (in seconds).
evaluationSampleRate = 0.01
-- | The sample rate used when visually comparing a chord annotation with a 
-- ground-truth annotation. Often a higher sample rate is preferred. Although
-- one uses precision, the visual result is easier to read.
displaySampleRate    = 0.3
-}
--------------------------------------------------------------------------------
-- A very small logic for comparing results including a wildcard (Ignore)
--------------------------------------------------------------------------------

-- A datatype for comparisons that can be equal, not equal, or ignored  
data EqIgnore = Equal   -- ^ Equal
              | NotEq   -- ^ Not equal
              | Ignore  -- ^ Ignored
                deriving (Eq, Show)
                
showEqi :: EqIgnore -> String
showEqi Equal  = "==*"
showEqi NotEq  = "/=*"
showEqi Ignore = "***"

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

--------------------------------------------------------------------------------
-- Summarising the durations EqIgnores
--------------------------------------------------------------------------------

data EqIDur = EqIDur { equals  :: Double
                     , notEqs  :: Double
                     , ignores :: Double } deriving (Eq, Show)


durations :: [Timed EqIgnore] -> EqIDur
durations = foldr step (EqIDur 0 0 0) where
  
  step :: Timed EqIgnore -> EqIDur -> EqIDur
  step t d = case getData t of
               Equal  -> d {equals  = equals  d + duration t}
               NotEq  -> d {notEqs  = notEqs  d + duration t}
               Ignore -> d {ignores = ignores d + duration t}
                     
totalDur, noIgnoreDur :: EqIDur -> Double
totalDur (EqIDur e n i)    = e + n + i
noIgnoreDur (EqIDur e n i) = e + n

sumDur :: [EqIDur] -> EqIDur
sumDur = foldr1 step where
  
  step :: EqIDur -> EqIDur -> EqIDur
  step (EqIDur e n i) (EqIDur e2 n2 i2) = EqIDur (e + e2) (n + n2) (i + i2)

overlapRatio :: [Timed EqIgnore] -> Double
overlapRatio es = let dur = durations es
                  in equals dur / noIgnoreDur dur

overlapRatioCol :: [[Timed EqIgnore]] -> Double
overlapRatioCol es = sum (map overlapRatio es) / genericLength es

weightOverlapRatio :: [[Timed EqIgnore]] -> Double
weightOverlapRatio es = let dur = sumDur . map durations $ es
                        in equals dur / noIgnoreDur dur
      
printAvgWOR :: Show b => String -> (a -> b) -> a -> IO (b)
printAvgWOR name ef es = do let r = ef es
                            putStrLn (name ++ ": " ++ show r)
                            return r
      
reportAvgWOR :: [[Timed EqIgnore]] -> IO (Double)
reportAvgWOR es = 
  let (EqIDur e n i) = sumDur . map durations $ es
  in do putStrLn "==============================================="
        putStrLn $ printf "Total duration equal:          \t%5.2f" e
        putStrLn $ printf "Total duration not equal:      \t%5.2f" n
        putStrLn $ printf "Total duration ignored:        \t%5.2f" i
        putStrLn $ printf "Total duration without ignored:\t%5.2f" (e+n)
        putStrLn $ printf "Total duration with ignored:   \t%5.2f" (e+n+i)
        let wcsr = weightOverlapRatio es
        putStrLn $ printf "Chord sequence recall:         \t%.6f" (overlapRatioCol es)
        putStrLn $ printf "Weighted chord sequence recall:\t%.6f\n" wcsr
        return wcsr
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

                       {-
-- | Returns True if both 'ChordLabel's are equal at the triad level: they are
-- either moth major or both minor. "None Chords" match only with other "None
-- Chords" and with nothing else
triadEq :: ChordLabel -> ChordLabel -> EqIgnore
triadEq a b =   chordRoot a  `rootEq` chordRoot b &&* triadEqI a b where

  triadEqI x y = case (toTriad x, toTriad y) of
                   (NoTriad, _      ) -> Ignore
                   (_      , NoTriad) -> Ignore
                   (tx     , ty     ) -> tx ==* ty 
-}
                   
-- compares the 'NoChord' and 'UndefChord' cases, such that this does not have
-- to be replicated in all Eq's
chordCompare :: (Root -> Root -> EqIgnore) 
             -> (RefLab -> ChordLabel -> EqIgnore) 
             ->  RefLab -> ChordLabel -> EqIgnore
chordCompare rEq cEq rf t = case (refLab rf, t) of
                             (NoChord,    NoChord   ) -> Equal
                             -- (UndefChord, NoChord   ) -> Ignore
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
  let -- Replaced the bassnote by the first inversion (root note)      
      -- removeBass :: ChordLabel -> ChordLabel
      -- removeBass (Chord r sh add _b) = Chord r sh add (Note Nat I1)

      bassMatch :: IntSet -> ChordLabel -> Int
      bassMatch _ (Chord _ _ _ (Note Nat I1)) = 0
      bassMatch p c | bassPC c `member` p     = 1
                    | otherwise               = 0
                   
      gtpc  = pc . toPitchClasses $ gt
      tstpc = pc . toPitchClasses $ test
      
      i     = gtpc `intersection` tstpc
      p     = size i
      p' = p + bassMatch gtpc test + bassMatch tstpc gt
      
  in case (chordShorthand gt, chordShorthand test) of
      (Aug, _  ) -> p' >=* 2
      (Dim, _  ) -> p' >=* 2
      _          -> p' >=* 3

--------------------------------------------------------------------------------
-- Evaluation functions
--------------------------------------------------------------------------------  
 {- 
-- | Calculates the relative correct overlap, which is the recall
-- of matching frames, and defined as the nr of matching frames (sampled at
-- an 10 millisecond interval) divided by all frames. The first argument 
-- specifies the kind of equality, the second argument should be a
-- reference ground truth annotation, and the third argument specifies the
-- evaluated (machine) annotation.
relCorrectOverlap :: (RefLab -> ChordLabel -> EqIgnore) 
                  -> [Timed RefLab] -> [Timed ChordLabel] 
                  -> Double
relCorrectOverlap eq gt test = 
  let samgt = sample gt
      samt  = sample test 
  in case  maxCompare eq samgt of
       0 -> 0 -- just output 0 of the maximum match is 0
       m -> foldr countMatch 0 (zipWith eq samgt samt) / m

countMatch :: EqIgnore -> Double -> Double
countMatch Equal x = succ x -- count the number of matching frames
countMatch _     x = x

-- Returns the maximal number of elements that can be correctly annotated.
-- Given an 'EqIgnore' equality function, it compares a sequence to itself.
-- Next we all 'Ignore's are removed and the length of the list is returned
maxCompare :: Num n => (RefLab -> ChordLabel -> EqIgnore) -> [RefLab] -> n
maxCompare eq gt = genericLength . filter (not . ignore)
                                 $ zipWith eq gt (map refLab gt)
             
-- | Given a chord annotation sample the chord label at every 10 ms
sample :: [Timed a]-> [a]
sample = sampleWith evaluationSampleRate

-- like sample, but takes a sample rate (seconds :: Float) as argument
sampleWith :: NumData -> [Timed a] -> [a]
sampleWith rate =  sampleAt [0.00, rate .. ] 

        
-- samples at specific points in time, specified in a list
sampleAt :: [NumData] -> [Timed a] -> [a]
sampleAt  _  [] = [] -- below, will never occur
sampleAt []  _  = error "Harmtrace.Audio.Evaluation: No sampling grid specified" 
sampleAt (t:ts) (c:cs)
  | t <= offset c = getData c : sampleAt ts (c:cs)
  | otherwise     = sampleAt (t:ts) cs         

  -}
  
totalDurationCheck :: [Timed RefLab] -> [Timed ChordLabel] -> Double
totalDurationCheck refs _ = offset . last $ refs
  
-- TODO rename to chord symbol recall
  
overlapEval :: (RefLab -> ChordLabel -> EqIgnore) 
            -> [Timed RefLab] -> [Timed ChordLabel] 
            -> [Timed EqIgnore] 
overlapEval eq gt test = map eval $ crossSegResetFst gt test where
  
  eval :: Timed (RefLab, ChordLabel) -> (Timed EqIgnore)
  eval dat = let (g,t) = getData dat in fmap (const (g `eq` t)) $ dat
  
printOverlapEval :: (RefLab -> ChordLabel -> EqIgnore) 
                 -> [Timed RefLab] -> [Timed ChordLabel] 
                 -> IO [Timed EqIgnore] 
printOverlapEval eq gt test = mapM eval $ crossSegResetFst gt test where
  
  eval :: Timed (RefLab, ChordLabel) -> IO (Timed EqIgnore)
  eval dat = do let (g,t) = getData dat
                    tstr  = printf "%.3f\t%.3f: " (onset dat) (offset dat)
                m <- printEqStr eq tstr g t
                return . fmap (const m) $ dat

-- | Checks whether the first Timed element have the same onset, and applies
-- chrossSegment.
crossSegResetFst :: [Timed RefLab] -> [Timed ChordLabel] 
                 -> [Timed (RefLab, ChordLabel)]
crossSegResetFst [] _ = []
crossSegResetFst _ [] = []
crossSegResetFst gt ts
  | og == ot  = crossSegment gt ts
  | og <  ot  = crossSegment -- crossSegResetFst 
                gt (Timed UndefChord          [Time og, Time ot] : ts)
  | otherwise = crossSegment -- crossSegResetFst 
                   (Timed (RefLab UndefChord) [Time ot, Time og] : gt) ts
      where  og = onset . head $ gt
             ot = onset . head $ ts
             
                
-- | Takes to Timed sequences and returns a Timed segment that zips
-- the data stored in the sequences using the segmentation of both sequences:
-- 
-- >>> crossSegment [ Timed "a" [Time 0, Time 4]
--                  , Timed "b" [Time 4, Time 7]] 
--                  [ Timed "1" [Time 0, Time 5]]
-- >>> [ Timed {getData = ("a","1"), getTimeStamps = [(0.0),(4.0)]}
--     , Timed {getData = ("b","1"), getTimeStamps = [(4.0),(5.0)]}]
--
-- N.B. We assume both sequences start at 0.0
crossSegment :: (Show a, Show b ) => [Timed a] -> [Timed b] 
             -> [Timed (a, b)]
crossSegment []  _    = []
crossSegment _  []    = []
crossSegment (g:gt) (t:ts)
  | og == ot  = fmap r g : crossSegment     gt     ts
  | og <  ot  = fmap r g : crossSegment     gt (rt:ts)
  | otherwise = fmap r t : crossSegment (rg:gt)    ts
    where  
           og = offset g
           ot = offset t
           rt = snd $ splitTimed t og
           rg = snd $ splitTimed g ot
           r  = const (getData g, getData t)

  
-- | calculates the number of chord changes in the ground-truth divided 
-- by the number of chord changes in the machine annotation. A number < 1 
-- indicates that the machine annotation misses some chord changes. A number
-- > 1 indicates that the machine annotation finds to many chord sequences.
chordChangeRatio ::  (ChordLabel -> ChordLabel -> EqIgnore) 
                 -> [Timed ChordLabel] -> [Timed ChordLabel] -> Double
chordChangeRatio eqi gt ma = (fromIntegral . countChordChanges $ gt)
                           / (fromIntegral . countChordChanges $ ma) where

  countChordChanges :: [Timed ChordLabel] -> Int
  countChordChanges cs = execState (foldrM step [] $ dropTimed cs) 0 

  step :: ChordLabel -> [ChordLabel] -> State Int [ChordLabel]
  step c []      = do modify succ
                      return [c]
  step a ( b : cs ) 
    | equal (a `eqi` b)  =    return (a : b : cs)
    | otherwise          = do modify succ
                              return (a : b : cs)

-- | The 'chordChangeRatio' is optimal if it is one, but it can be larger or 
-- smaller than 1. Therefore, calculating the average blurs the actual result.
-- 'avgDistToOne' takes the absolute difference to 1.0 and averages these for a
-- list of Doubles.
avgDistToOne :: [Double] -> Double
avgDistToOne ds = (sum . map absDistToOne $ ds) / genericLength ds where

  absDistToOne :: Double -> Double
  absDistToOne a = abs (1.0 - a)

--------------------------------------------------------------------------------
-- Displaying evaluations (all in IO)
--------------------------------------------------------------------------------    

-- | Takes an 'EqIgnore' equality and a String and returns the same equality
-- function but wrapped in IO. At every evaluation the evaluation is printed
-- to the user. The String is prefixed to this output.
printEqStr :: (RefLab -> ChordLabel -> EqIgnore) 
           -> String -> RefLab -> ChordLabel -> IO (EqIgnore)
printEqStr eqf str gt test = 
  do let eqi = gt `eqf` test
     putStrLn . (str ++) . intercalate " " $ [show gt, showEqi eqi, show test]
     return eqi

 
   {-
-- | Calculates the relative correct overlap, which is the recall
-- of matching frames, and defined as the nr of matching frames (sampled at
-- an interval set in 'ChordTrack.Constants') divided by all frames.
-- This functions differs from 'relCorrectOverlap' in that it runs in IO and
-- prints the comparison to the user.
printRCO :: (RefLab -> ChordLabel -> EqIgnore) 
         -> [Timed RefLab] -> [Timed ChordLabel] -> IO (Double)
printRCO eqi gt test = 
  do let samgt = sampleWith displaySampleRate gt
         sam   = sampleWith displaySampleRate test
         pEq ts a b = printEqStr eqi (printf "%.2f: " ts) a b
         
     matches <- sequence $ zipWith3 pEq [0,displaySampleRate ..] samgt sam
     return (foldr countMatch 0 matches / maxCompare eqi samgt)
     -}
     
