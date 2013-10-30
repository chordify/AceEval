{-# OPTIONS_GHC -Wall          #-}
module ACE.Evaluation.Segmentation where

import ACE.Evaluation.ChordEq
import ACE.Evaluation.Func 

import HarmTrace.Base.Chord
import HarmTrace.Base.Time

import Text.Printf               ( printf )
-- import Debug.Trace

segmentEval :: [Timed RefLab] -> [Timed ChordLabel] -> (Double, Double)
segmentEval gt test = undefined 

  where (csGt, csTst) = unzipTimed $ crossSegment gt test 
  
-- hamDistUnderSegVerb :: [Timed RefLab] -> [Timed ChordLabel] 
                -- -> IO ([Double], Double)   
-- hamDistUnderSegVerb gt test = hammingDistVerb gt test

-- hamDistOverSegVerb :: [Timed RefLab] -> [Timed ChordLabel]
                   -- -> IO ([Double], Double)   
-- hamDistOverSegVerb gt test = hammingDistVerb test gt

-- | Calculates the directional hamming distance for two sequences, like
-- 'hammingDist', but verbosely prints the result of the segmentation 
-- evaluation. 
hammingDistVerb :: (Show a, Show b ) => [Timed a] -> [Timed b] 
                -> IO ([Double], Double)
hammingDistVerb x y = do let csb = snd . unzipTimed $ crossSegment x y
                         r <- sequence $ hammingDist' durAllButMaxVerb x csb
                         return ( r, getEndTime x ) where
                           
  durAllButMaxVerb :: (Show a, Show b ) => Timed a -> [Timed b] -> IO Double
  durAllButMaxVerb gt ts = mapM (printDur gt) ts >>= return . sum where 

    m = maximum (map duration ts)
    
    printDur :: (Show a, Show b) => Timed a -> Timed b -> IO Double
    printDur a b 
      | duration b == m = do putStr (showSeg  b)
                             putStrLn (show (getData a) ++ " *** " 
                                    ++ show (getData b)) 
                             return 0
      | otherwise       = do let d = duration b
                             putStr (showSeg  b)
                             putStrLn (show (getData a) ++ " <-> " 
                                    ++ show (getData b) ++ " " ++ show d) 
                             return d
 
  showSeg :: Timed b -> String
  showSeg b = printf "%3.3f\t%3.3f: " (onset b) (offset b)
                         
-- | Calculates the directional hamming distance for two sequences per segment
-- the right part of the tuple is the final length of the sequence for 
-- normalisation
hammingDist :: (Show a, Show b ) => [Timed a] -> [Timed b] -> ([Double], Double)
hammingDist a b = 
  ( hammingDist' durAllButMax a (snd . unzipTimed $ crossSegment a b)
  , getEndTime a ) where
                            
      durAllButMax :: a -> [Timed b] -> Double
      durAllButMax _ td = let d = map duration td in (sum d) - (maximum d)
  
hammingDist' :: (Show a, Show b ) => (Timed a -> [Timed b] -> c) 
             -> [Timed a] -> [Timed b] -> [c]
hammingDist' _ [] [] = []
hammingDist' _ []  _ = error "hammingDist': comparing sequences of different lengths"
hammingDist' _ _  [] = error "hammingDist': comparing sequences of different lengths"
hammingDist' mxf (g:gt) tst = mxf g t : hammingDist' mxf gt ts
    where  (t,ts) = span (\x -> offset x <= offset g) tst

-- | Normalises the results of 'hammingDist' and 'hammingDistVerb' returning
-- the actual directional Hamming distance
normHamDist :: ([Double], Double) -> Double
normHamDist (hd, totLen) = (sum hd) / totLen


                      
unzipTimed :: [Timed (a,b)] -> ([Timed a], [Timed b])  
unzipTimed = unzip . map liftTimedTuple
  
liftTimedTuple :: Timed (a,b) -> (Timed a, Timed b)
liftTimedTuple td = let (a,b) = getData td in (setData td a, setData td b)
