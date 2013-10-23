
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


module ACE.Evaluation.Func (
    -- * Evaluation functions
      -- relCorrectOverlap
      chordChangeRatio
    , avgDistToOne
    , overlapEval
    , overlapRatio
    , overlapRatioCol
    , weightOverlapRatio
    , reportAvgWOR
    
    , printOverlapEval
    ) where

import ACE.Evaluation.EqIgnore
import ACE.Evaluation.ChordEq
    
import HarmTrace.Base.Time 
import HarmTrace.Base.Chord 

import Data.List                 ( genericLength, intercalate )

import Data.Foldable             ( foldrM )
import Control.Monad.State       ( State, execState, modify )
import Text.Printf               ( printf )

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
                     
noIgnoreDur :: EqIDur -> Double
noIgnoreDur (EqIDur e n _i) = e + n

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
-- Evaluation functions
--------------------------------------------------------------------------------  
  
-- TODO rename to chord symbol recall
  
overlapEval :: (RefLab -> ChordLabel -> EqIgnore) 
            -> [Timed RefLab] -> [Timed ChordLabel] 
            -> [Timed EqIgnore] 
overlapEval eq gt test = map eval $ crossSegment gt test where
  
  eval :: Timed (RefLab, ChordLabel) -> (Timed EqIgnore)
  eval dat = let (g,t) = getData dat in fmap (const (g `eq` t)) $ dat
  
printOverlapEval :: (RefLab -> ChordLabel -> EqIgnore) 
                 -> [Timed RefLab] -> [Timed ChordLabel] 
                 -> IO [Timed EqIgnore] 
printOverlapEval eq gt test = mapM eval $ crossSegment gt test where
  
  eval :: Timed (RefLab, ChordLabel) -> IO (Timed EqIgnore)
  eval dat = do let (g,t) = getData dat
                    tstr  = printf "%.3f\t%.3f: " (onset dat) (offset dat)
                m <- printEqStr eq tstr g t
                return . fmap (const m) $ dat
         
                
-- | Takes to Timed sequences and returns a Timed segment that zips
-- the data stored in the sequences using the segmentation of both sequences:
-- 
-- >>> crossSegment [ Timed "a" [Time 0, Time 4]
--                  , Timed "b" [Time 4, Time 7]] 
--                  [ Timed "1" [Time 0, Time 5]]
-- >>> [ Timed {getData = ("a","1"), getTimeStamps = [(0.0),(4.0)]}
--     , Timed {getData = ("b","1"), getTimeStamps = [(4.0),(5.0)]}]
--
-- N.B. We assume both sequences start at the same time
crossSegment :: (Show a, Show b ) => [Timed a] -> [Timed b] 
             -> [Timed (a, b)]
crossSegment [] []    = []
crossSegment []  _    = error "crossSegment: comparing sequences of different lengths"
crossSegment _  []    = error "crossSegment: comparing sequences of different lengths"
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

showEqi :: EqIgnore -> String
showEqi Equal  = "==*"
showEqi NotEq  = "/=*"
showEqi Ignore = "***"

 