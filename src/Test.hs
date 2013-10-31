module Main where

import HarmTrace.Base.Chord
import HarmTrace.Base.Parse
import HarmTrace.Base.Time

import ACE.Evaluation.Func (printOverlapEval, overlapRatioCCEval)
import ACE.Evaluation.ChordEq 


main :: IO ()
main = do putStrLn "AceEval Test:"
          let (a,b) = combine
          putStrLn "==* = equals,, /=* = not equal,, *** = ignored from evaluation"
          putStrLn "onset,offset,reference,root,maj/min,seventh,maj/maj inv, seventh inv,prediction"
          printOverlapEval chordClassEq (makeGT . addTime $ a) (addTime b) >>=
            print . overlapRatioCCEval

testChordSeq :: [ChordLabel]
testChordSeq = map (parseDataSafe pChord) 
  ["N", "X", "G:maj", "G:(1)", "G:min", "C#:7", "G:maj/b3", "G:maj/5", 
   "G:min7/5", "G:min/b3","G:sus4", "G:min7(11)", "G:dim7", "F##:7(s9)", 
   "Abb:(1,5)"]
   
combine :: ([ChordLabel], [ChordLabel])
combine = unzip $ [(x,y) | x <- testChordSeq, y <- testChordSeq]

addTime :: [a] -> [Timed a]
addTime a = zipWith3 timed a [0.0 ..] [1.0 ..]

