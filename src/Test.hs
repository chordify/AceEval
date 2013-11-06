module Main where

import HarmTrace.Base.Chord
import HarmTrace.Base.Parse
import HarmTrace.Base.Time

import ACE.Evaluation.Func (printOverlapEval, overlapRatioCCEval)
import ACE.Evaluation.ChordEq 
import ACE.Evaluation.Segmentation 

import System.Environment (getArgs)

main :: IO ()
main = do a <- getArgs
          case a of 
            ["1"] -> do putStrLn "AceEval Test:"
                        let (a,b) = combine
                        putStrLn "==* = equals,, /=* = not equal,, *** = ignored from evaluation"
                        putStrLn "onset,offset,reference,root,maj/min,seventh,maj/maj inv, seventh inv,prediction"
                        printOverlapEval chordClassEq (makeGT . addTime $ a) (addTime b) >>=
                          print . overlapRatioCCEval
            ["2"] -> do putStrLn "2"
                        let x = makeGT (fromInt [3])
                            y = fromInt [1,2]
                        putStrLn (prettyPrint x) 
                        putStrLn (prettyPrint y)
                        print (segmentEval x y)
                        hamDistUnderSegVerb x y
                        return ()
            _     -> error "please enter 1 or 2..."

  
-- 1
testChordSeq :: [ChordLabel]
testChordSeq = map (parseDataSafe pChord) 
  ["N", "X", "G:maj", "G:(1)", "G:min", "C#:7", "G:maj/b3", "G:maj/5", 
   "G:min7/5", "G:min/b3","G:sus4", "G:min7(11)", "G:dim7", "F##:7(s9)", 
   "Abb:(1,5)"]
   
combine :: ([ChordLabel], [ChordLabel])
combine = unzip $ [(x,y) | x <- testChordSeq, y <- testChordSeq]

addTime :: [a] -> [Timed a]
addTime a = zipWith3 timed a [0.0 ..] [1.0 ..]


-- 2

fromInt :: [Double] -> [Timed ChordLabel]
fromInt = reverse . foldr step [] where

  d = parseDataSafe pChord "C:maj"
  
  step :: Double -> [Timed ChordLabel] -> [Timed ChordLabel]
  step i []    = [ timed d 0 i ]   
  step i (h:t) = let x = offset h in timed d x (i+x) : h : t