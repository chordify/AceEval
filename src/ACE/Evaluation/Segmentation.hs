{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE DeriveFunctor        #-}
module ACE.Evaluation.Segmentation ( SegEval (..)
                                   , reportSegment
                                   , segmentEval
                                   , hamDistOverSeg
                                   , hamDistOverSegVerb
                                   , hamDistUnderSeg
                                   , hamDistUnderSegVerb
                                   -- , normHamDist
                                   -- , normSegEval
                                   , average
                                   ) where

import ACE.Evaluation.ChordEq
import ACE.Evaluation.Func

import HarmTrace.Base.Chord
import HarmTrace.Base.Time

import Text.Printf               ( printf )
import Data.List                 ( genericLength )

data SegEval a = SegEval { underSeg :: a -- under segmentation score 1- d(gt,test)
                         , overSeg  :: a -- over  segmentation score 1- d(test,gt)
                         , segScore :: a -- the final segmenation score (min of the above)
                         } deriving (Eq, Functor)

instance Show a => Show (SegEval a) where
  show (SegEval u o l) = unwords . map show $ [u,o,l]

sequenceSegEval :: [SegEval a] -> SegEval [a]
sequenceSegEval = foldr step (SegEval [] [] []) where
  step :: SegEval b -> SegEval [b] -> SegEval [b]
  step (SegEval u o l) (SegEval us os ls) = SegEval (u:us) (o:os) (l:ls)


-- teamSegmentation :: (SegEval Double -> Double) -> [SegEval Double] -> Double
-- teamSegmentation f = average . map f

reportSegment :: [SegEval Double] -> IO ()
reportSegment se =
  do let (SegEval us os mxs) = fmap average . sequenceSegEval $ se

     putStrLn  "================================================"
     putStrLn ("under segmentation          : " ++ show us )
     putStrLn ("over segmentation           : " ++ show os )
     putStrLn ("average segmentation quality: " ++ show mxs ++ "\n")

-- csvSegment :: [SegEval Double] -> IO ()
-- csvSegment se =
--   do let (SegEval us os mxs) = fmap average . sequenceSegEval $ se
--      putStrLn . intercalate "," . map show $ [us, os, mxs]

segmentEval :: [Timed RefLab] -> [Timed ChordLabel] -> SegEval Double
segmentEval gt test =
  let (csGt, csTst) = unzipTimed $! crossSegment gt test
      under = 1 - (sum (hammingDist' durAllButMax gt   csTst)
            / realToFrac (getEndTime gt))
      over  = 1 - (sum (hammingDist' durAllButMax test csGt )
            / realToFrac (getEndTime test))
  in under `seq` over `seq` SegEval under over (min under over)

hamDistUnderSegVerb :: [Timed RefLab] -> [Timed ChordLabel] -> IO Double
hamDistUnderSegVerb gt test = hammingDistVerb gt test

hamDistOverSegVerb :: [Timed RefLab] -> [Timed ChordLabel] -> IO Double
hamDistOverSegVerb gt test = hammingDistVerb test gt

hamDistUnderSeg :: [Timed RefLab] -> [Timed ChordLabel] -> Double
hamDistUnderSeg gt test = hammingDist gt test

hamDistOverSeg :: [Timed RefLab] -> [Timed ChordLabel] -> Double
hamDistOverSeg gt test = hammingDist test gt

-- | Calculates the directional hamming distance for two sequences, like
-- 'hammingDist', but verbosely prints the result of the segmentation
-- evaluation.
hammingDistVerb :: (Show a, Show b ) => [Timed a] -> [Timed b] -> IO Double
hammingDistVerb x y = do let csb = snd . unzipTimed $ crossSegment x y
                         r <- sequence $ hammingDist' durAllButMaxVerb x csb
                         return ( 1 - ((sum r)
                                /  realToFrac (getEndTime x ))) where

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
                             return . realToFrac $ d

  showSeg :: Timed b -> String
  showSeg b = printf "%3.3f,%3.3f," (onset b) (offset b)

-- | Calculates the directional hamming distance for two sequences per segment
-- the right part of the tuple is the final length of the sequence for
-- normalisation
hammingDist :: (Show a, Show b ) => [Timed a] -> [Timed b] -> Double
hammingDist a b = let csB = snd . unzipTimed $ crossSegment a b
                  in 1 - ( sum (hammingDist' durAllButMax a csB)
                     / realToFrac (getEndTime a))

-- N.B the first argument is not used here, but it is used in the verbose
-- version. Hence, to be able to use hammingDist' we put it here as well
durAllButMax :: a -> [Timed b] -> Double
durAllButMax _ [] = error "durAllButMax: empty list"
durAllButMax _ td = let d = map (realToFrac . duration) td
                    in (sum d) - (maximum d)

-- Does the actual work for the hamming distance functions
hammingDist' :: (Show a, Show b ) => (Timed a -> [Timed b] -> c)
             -> [Timed a] -> [Timed b] -> [c]
hammingDist' _ [] [] = []
hammingDist' _ []  _ = error "hammingDist': comparing sequences of different lengths"
hammingDist' _ _  [] = error "hammingDist': comparing sequences of different lengths"
hammingDist' mxf (g:gt) tst = mxf g t : hammingDist' mxf gt ts
    where  (t,ts) = span (\x -> offset x <= offset g) tst

--------------------------------------------------------------------------------
-- Utilities (move to HarmTrace.Base.Time???)
--------------------------------------------------------------------------------

-- TODO move somewhere else
-- | Calculated the average of list of numbers
average :: Fractional a => [a] -> a
average [] = error "average: empty list"
average l  = sum l / genericLength l

unzipTimed :: [Timed (a,b)] -> ([Timed a], [Timed b])
unzipTimed = unzip . map liftTimedTuple

liftTimedTuple :: Timed (a,b) -> (Timed a, Timed b)
liftTimedTuple td = let (a,b) = getData td in (setData td a, setData td b)
