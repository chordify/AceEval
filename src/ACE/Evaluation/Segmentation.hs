{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE DeriveFunctor        #-}
module ACE.Evaluation.Segmentation ( SegEval (..)
                                   , reportSegment
                                   , segmentEval
                                   -- , hamDistOverSeg
                                   -- , hamDistOverSegVerb
                                   , hamDistUnderSeg
                                   -- , hamDistUnderSegVerb
                                   , normHamDist
                                   , normSegEval
                                   ) where

import ACE.Evaluation.ChordEq
import ACE.Evaluation.Func 

import HarmTrace.Base.Chord
import HarmTrace.Base.Time

import Text.Printf               ( printf )
import Data.List                 ( intercalate, genericLength )

data SegEval a = SegEval a -- under segmentation score d(gt,test)
                         a -- over  segmentation score d(test,gt)
                         a -- total duration of the ground truth
                         deriving (Eq, Functor)
                       
instance Show a => Show (SegEval a) where
  show (SegEval u o l) = intercalate " " . map show $ [u,o,l] 
  
sequenceSegEval :: [SegEval a] -> SegEval [a]
sequenceSegEval = foldr step (SegEval [] [] []) where
  step :: SegEval b -> SegEval [b] -> SegEval [b] 
  step (SegEval u o l) (SegEval us os ls) = SegEval (u:us) (o:os) (l:ls)

reportSegment :: [SegEval Double] -> IO () 
reportSegment se = 
  do let (SegEval us os mxs) = fmap average . sequenceSegEval . map normSegEval $ se
         n                   = genericLength se
         
         average [] = error "average: empty list"
         average l  = sum l / n
         
     putStrLn  "================================================"
     putStrLn ("under segmentation          : " ++ show us ) 
     putStrLn ("over segmentation           : " ++ show os ) 
     putStrLn ("average segmentation quality: " ++ show mxs ++ "\n")  
  
segmentEval :: [Timed RefLab] -> [Timed ChordLabel] -> SegEval Double
segmentEval gt test = 
  let (csGt, csTst) = unzipTimed $ crossSegment gt test 
      under = 1 - (sum (hammingDist' durAllButMax gt   csTst) / getEndTime gt  )
      over  = 1 - (sum (hammingDist' durAllButMax test csGt ) / getEndTime test)
  in SegEval under over (min under over)
      
-- hamDistUnderSegVerb :: [Timed RefLab] -> [Timed ChordLabel] -> Double
                -- -> IO ([Double], Double)   
-- hamDistUnderSegVerb gt test = hammingDistVerb gt test

-- hamDistOverSegVerb :: [Timed RefLab] -> [Timed ChordLabel]
                   -- -> IO ([Double], Double)   
-- hamDistOverSegVerb gt test = hammingDistVerb test gt

hamDistUnderSeg :: [Timed RefLab] -> [Timed ChordLabel] -> Double
hamDistUnderSeg gt test = hammingDist gt test

-- hamDistOverSeg :: [Timed RefLab] -> [Timed ChordLabel] -> ([Double], Double)   
-- hamDistOverSeg gt test = hammingDist test gt

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
  showSeg b = printf "%3.3f,%3.3f," (onset b) (offset b)
                         
-- | Calculates the directional hamming distance for two sequences per segment
-- the right part of the tuple is the final length of the sequence for 
-- normalisation
hammingDist :: (Show a, Show b ) => [Timed a] -> [Timed b] -> Double
hammingDist a b = let csB = snd . unzipTimed $ crossSegment a b
                  in 1 - ( sum (hammingDist' durAllButMax a csB) / getEndTime a)
                    
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
normHamDist (hd, totLen) =  1 - ( (sum hd) / totLen ) 

-- | Normalises the results of 'hammingDist', /d/, when it has been applied
-- for under segmentation /d(gt,test)/ and over segmentation /d(test,gt)/ 
-- returning both normalised directional Hamming distance and their maximum
normSegEval :: SegEval Double -> SegEval Double
normSegEval (SegEval u o totLen) = let dus = 1 - ( u / totLen )
                                       dos = 1 - ( o / totLen )
                                   in SegEval dus dos (min dus dos)

                      
unzipTimed :: [Timed (a,b)] -> ([Timed a], [Timed b])  
unzipTimed = unzip . map liftTimedTuple
  
liftTimedTuple :: Timed (a,b) -> (Timed a, Timed b)
liftTimedTuple td = let (a,b) = getData td in (setData td a, setData td b)
