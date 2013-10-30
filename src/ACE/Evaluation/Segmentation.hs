{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE DeriveFunctor        #-}
module ACE.Evaluation.Segmentation where

import ACE.Evaluation.ChordEq
import ACE.Evaluation.Func 

import HarmTrace.Base.Chord
import HarmTrace.Base.Time

import Text.Printf               ( printf )
import Data.List                 ( intercalate )

-- import Debug.Trace

-- myTrace (a,b) = trace (myShow a ++ myShow b) (a,b)  where
  
  -- myShow :: Show a => [a] -> String
  -- myShow = intercalate "\n" . map show 

data SegEval a = SegEval a -- under segmentation score d(gt,test)
                         a -- over  segmentation score d(test,gt)
                         a -- total duration of the ground truth
                         deriving (Eq, Functor)
                       
instance Show a => Show (SegEval a) where
  show (SegEval u o l) = intercalate " " . map show $ [u,o,l] 

segmentEval :: [Timed RefLab] -> [Timed ChordLabel] -> SegEval Double
segmentEval gt test = 
  let (csGt, csTst) = unzipTimed $ crossSegment gt test 
  in  SegEval (sum $ hammingDist' durAllButMax gt   csTst)
              (sum $ hammingDist' durAllButMax test csGt)
              (getEndTime gt)
  
hamDistUnderSegVerb :: [Timed RefLab] -> [Timed ChordLabel] 
                -> IO ([Double], Double)   
hamDistUnderSegVerb gt test = hammingDistVerb gt test

hamDistOverSegVerb :: [Timed RefLab] -> [Timed ChordLabel]
                   -> IO ([Double], Double)   
hamDistOverSegVerb gt test = hammingDistVerb test gt

hamDistUnderSeg :: [Timed RefLab] -> [Timed ChordLabel] -> ([Double], Double)   
hamDistUnderSeg gt test = hammingDist gt test

hamDistOverSeg :: [Timed RefLab] -> [Timed ChordLabel] -> ([Double], Double)   
hamDistOverSeg gt test = hammingDist test gt

-- | Calculates the directional hamming distance for two sequences, like
-- 'hammingDist', but verbosely prints the result of the segmentation 
-- evaluation. 
hammingDistVerb :: (Show a, Show b ) => [Timed a] -> [Timed b] 
                -> IO ([Double], Double)
hammingDistVerb x y = do let csb = snd . unzipTimed $ crossSegment x y
                         r <- sequence $ hammingDist' durAllButMaxVerb x csb
                         return ( r, getEndTime x ) where
                           
  durAllButMaxVerb :: (Show a, Show b ) => Timed a -> [Timed b] -> IO Double
  durAllButMaxVerb _ [] = error "durAllButMaxVerb: empty list"
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
  , getEndTime a )
                    
-- N.B the first argument is not used here, but it is used in the verbose 
-- version. Hence, to be able to use hammingDist' we put it here as well
durAllButMax :: a -> [Timed b] -> Double
durAllButMax _ [] = error "durAllButMax: empty list"
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

-- | Normalises the results of 'hammingDist', /d/, when it has been applied
-- for under segmentation /d(gt,test)/ and over segmentation /d(test,gt)/ 
-- returning both normalised directional Hamming distance and their maximum
normSegEval :: SegEval Double -> SegEval Double
normSegEval (SegEval u o totLen) = let dus = u / totLen
                                       dos = o / totLen
                                   in SegEval dus dos (max dus dos)

                      
unzipTimed :: [Timed (a,b)] -> ([Timed a], [Timed b])  
unzipTimed = unzip . map liftTimedTuple
  
liftTimedTuple :: Timed (a,b) -> (Timed a, Timed b)
liftTimedTuple td = let (a,b) = getData td in (setData td a, setData td b)
