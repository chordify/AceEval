{-# OPTIONS_GHC -Wall          #-}
module ACE.Evaluation.Segmentation where

import ACE.Evaluation.ChordEq
import ACE.Evaluation.Func 

import HarmTrace.Base.Chord
import HarmTrace.Base.Time

segmentEval :: [Timed RefLab] -> [Timed ChordLabel] -> (Double, Double)
segmentEval gt test = undefined 

  where (csGt, csTst) = unzipTimed $ crossSegment gt test 
  
hammingDistVerb :: (Show a, Show b ) => [Timed a] -> [Timed b] -> IO [Double]
hammingDistVerb a b = 
  sequence $ hammingDist' durAllButMaxVerb a (snd . unzipTimed $ crossSegment a b)
  
hammingDist :: (Show a, Show b ) => [Timed a] -> [Timed b] -> [Double]
hammingDist a b = 
  hammingDist' durAllButMax a (snd . unzipTimed $ crossSegment a b)
  
hammingDist' :: (Show a, Show b ) => (Timed a -> [Timed b] -> c) 
             -> [Timed a] -> [Timed b] -> [c]
hammingDist' _ [] [] = []
hammingDist' _ []  _ = error "hammingDist': comparing sequences of different lengths"
hammingDist' _ _  [] = error "hammingDist': comparing sequences of different lengths"
hammingDist' mxf (g:gt) tst = mxf g t : hammingDist' mxf gt ts
    where  (t,ts) = span (\x -> offset x == offset g) tst

normHamDist :: [Timed a] -> [Double] -> Double
normHamDist td hd = (sum hd) / (offset . last $ td)

durAllButMaxVerb :: (Show a, Show b ) => Timed a -> [Timed b] -> IO Double
durAllButMaxVerb gt ts = mapM (printDur gt) ts >>= return . sum

  where m = maximum (map duration ts)
  
        printDur :: (Show a, Show b) => Timed a -> Timed b -> IO Double
        printDur a b 
          | duration b == m = putStrLn (show a ++ " *** " ++ show b) >> return 0
          | otherwise       = putStrLn (show a ++ " <-> " ++ show b) 
                            >> return (duration b)
                        
durAllButMax :: a -> [Timed b] -> Double
durAllButMax _ td = let d = map duration td in (sum d) - (maximum d)
  
unzipTimed :: [Timed (a,b)] -> ([Timed a], [Timed b])  
unzipTimed = unzip . map liftTimedTuple
  
liftTimedTuple :: Timed (a,b) -> (Timed a, Timed b)
liftTimedTuple td = let (a,b) = getData td in (setData td a, setData td b)
