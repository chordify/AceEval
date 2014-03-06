
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
      overlapEval
    , overlapRatio
    , overlapRatioCol
    , overlapDur
    , teamOverlapRatios
    , weightOverlapRatio
    , reportAvgWOR
    , reportMIREX13
    , csvMIREX13
    , csvPerSongForAllTeams
    , overlapRatioCCEval
    , printOverlapEval
    -- * Utilities
    , crossSegment
    ) where

import ACE.Evaluation.EqIgnore
import ACE.Evaluation.ChordEq
import ACE.Evaluation.ChordClass
    
import HarmTrace.Base.Time 
import HarmTrace.Base.Chord 

import Data.List                 ( genericLength, intercalate, transpose )
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
sumDur = foldr step (EqIDur 0 0 0) where
  
  step :: EqIDur -> EqIDur -> EqIDur
  step (EqIDur e n i) (EqIDur e2 n2 i2) = EqIDur (e + e2) (n + n2) (i + i2)

-- | Calculates the Chord Sequence Recall: the correct overlap between
-- the groundtruth chord annotation and the predicted annotation
overlapRatio :: [Timed EqIgnore] -> Double
overlapRatio es = let dur = durations es
                  in equals dur / noIgnoreDur dur

overlapDur :: [Timed EqIgnore] -> Double 
overlapDur es = let (EqIDur e ne i) = durations es in e + ne + i
                  
-- | Calculates and averages the Chord Sequence Recall for a list of evaluation
-- results 
overlapRatioCol :: [[Timed EqIgnore]] -> Double
overlapRatioCol es = sum (map overlapRatio es) / genericLength es

-- | Calculates the weighted Chord Sequence Recall
weightOverlapRatio :: [[Timed EqIgnore]] -> Double
weightOverlapRatio es = let dur = sumDur . map durations $ es
                        in equals dur / noIgnoreDur dur
                        
-- | Takes a comp
teamOverlapRatios :: ( CCEval (Timed EqIgnore) -> (Timed EqIgnore) ) 
                  -> [[Timed (CCEval EqIgnore)]] -> [Double]
teamOverlapRatios f = map (overlapRatio . map (f . unzipTimed) )

csvPerSongForAllTeams :: [[Double]] -> IO ()
csvPerSongForAllTeams d = 
  do putStrLn "" 
     mapM_ (putStrLn . intercalate "," . map show) . transpose $ d
     
reportAvgWOR :: [[Timed EqIgnore]] -> IO ()
reportAvgWOR es = 
  let (EqIDur e n i) = sumDur . map durations $ es
  in do putStrLn "================================================"
        putStrLn $ printf "Total duration equal:          \t%5.2f" e
        putStrLn $ printf "Total duration not equal:      \t%5.2f" n
        putStrLn $ printf "Total duration ignored:        \t%5.2f" i
        putStrLn $ printf "Total duration without ignored:\t%5.2f" (e+n)
        putStrLn $ printf "Total duration with ignored:   \t%5.2f" (e+n+i)
        let wcsr = weightOverlapRatio es
        putStrLn $ printf "Chord sequence recall:         \t%.6f" (overlapRatioCol es)
        putStrLn $ printf "Weighted chord sequence recall:\t%.6f\n" wcsr
        -- return wcsr
        

        
-- TODO perhaps nicer solved by implementing Traversable, and use sequenceA)
unzipTimed :: Functor f => Timed (f a) -> f (Timed a)
unzipTimed td = fmap (setData td) . getData $ td 

reportMIREX13 :: [[Timed (CCEval EqIgnore)]] -> IO () -- (CCEval Double)
reportMIREX13 ce = 
  do let (CCEval r m s im is) = fmap weightOverlapRatio . sequenceCCEval 
                              . map (sequenceCCEval . map unzipTimed) $ ce
     putStrLn  "================================================"
     putStrLn ("root                        : " ++ show r  ) 
     putStrLn ("major / minor               : " ++ show m  ) 
     putStrLn ("sevenths                    : " ++ show s  ) 
     putStrLn ("major / minor w. inversions : " ++ show im ) 
     putStrLn ("sevenths w. inversions      : " ++ show is ++ "\n" )

csvMIREX13 :: [[Timed (CCEval EqIgnore)]] -> IO () -- (CCEval Double)
csvMIREX13 ce = 
  do let (CCEval r m s im is) = fmap weightOverlapRatio . sequenceCCEval 
                              . map (sequenceCCEval . map unzipTimed) $ ce
     putStrLn . intercalate "," . map show $ [r,m,s,im,is]
     
overlapRatioCCEval :: [Timed (CCEval EqIgnore)] -> CCEval Double
overlapRatioCCEval = fmap overlapRatio . sequenceCCEval . map unzipTimed


--------------------------------------------------------------------------------
-- Evaluation functions
--------------------------------------------------------------------------------  
  
-- TODO rename to chord symbol recall
  
overlapEval :: (RefLab -> ChordLabel -> a) 
            -> [Timed RefLab] -> [Timed ChordLabel] 
            -> [Timed a] 
overlapEval eq gt test = map eval $ crossSegment gt test where
  
  -- eval :: Timed (RefLab, ChordLabel) -> (Timed a)
  eval dat = let (g,t) = getData dat in fmap (const (g `eq` t)) $ dat
  
printOverlapEval :: Show a => (RefLab -> ChordLabel -> a) 
                 -> [Timed RefLab] -> [Timed ChordLabel] 
                 -> IO [Timed a] 
printOverlapEval eq gt test = mapM eval $ crossSegment gt test where
  
  -- eval :: Timed (RefLab, ChordLabel) -> IO (Timed a)
  eval dat = do let (g,t) = getData dat
                    tstr  = printf "%.3f,%.3f," (onset dat) (offset dat)
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


--------------------------------------------------------------------------------
-- Displaying evaluations (all in IO)
--------------------------------------------------------------------------------    

-- | Takes an 'EqIgnore' equality and a String and returns the same equality
-- function but wrapped in IO. At every evaluation the evaluation is printed
-- to the user. The String is prefixed to this output.
printEqStr :: Show a => (RefLab -> ChordLabel -> a) 
           -> String -> RefLab -> ChordLabel -> IO (a)
printEqStr eqf str gt test = 
  do let eqi = gt `eqf` test
     putStrLn . (str ++) . intercalate "," $ [show (show gt), show eqi, show (show test)]
     return eqi
 
