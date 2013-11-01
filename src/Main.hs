{-# OPTIONS_GHC -Wall #-}
module Main where

import ACE.MIREX
import ACE.Evaluation


-- other libraries
import System.Console.ParseArgs
import System.FilePath            ( (</>) )
import System.Directory           ( doesDirectoryExist )
import System.IO                  ( stderr, Handle, openFile )


data MirexArgs = MirexDir | MirexFilepath | MirexYear | Print | Team | ID
               | Collection | GroundTruthDir | VocabularyMapping | FileFormat
               | ErrorStream
               deriving (Eq, Ord, Show)

myArgs :: [Arg MirexArgs]
myArgs = [
           Arg { argIndex = Print,
                 argAbbr  = Just 'p',
                 argName  = Just "print",
                 argData  = Nothing,
                 argDesc  = "switches AceEval in verbose mode"
               }  
         , Arg { argIndex = MirexDir,
                 argAbbr  = Just 'd',
                 argName  = Just "dir",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Base directory containing the mirex results"
               }               
         , Arg { argIndex = MirexFilepath,
                 argAbbr  = Just 'f',
                 argName  = Just "file",
                 argData  = argDataOptional "filepath" ArgtypeString,
                 argDesc  = "Input file to be evaluated"
               }
         , Arg { argIndex = MirexYear,
                 argAbbr  = Just 'y',
                 argName  = Just "year",
                 argData  = argDataRequired "year" ArgtypeString,
                 argDesc  = "The year of the MIREX iteration to be evaluated"
               }
         , Arg { argIndex = Collection,
                 argAbbr  = Just 'c',
                 argName  = Just "col",
                 argData  = argDataRequired "collection" ArgtypeString,
                 argDesc  = "The data collection used in the evaluation"
               }
         , Arg { argIndex = Team,
                 argAbbr  = Just 't',
                 argName  = Just "team",
                 argData  = argDataOptional "string" ArgtypeString,
                 argDesc  = "evaluates only a specific team"
               }         
         , Arg { argIndex = ID,
                 argAbbr  = Just 'i',
                 argName  = Just "id",
                 argData  = argDataOptional "int" ArgtypeInt,
                 argDesc  = "evaluates only a specific ID"
               } 
         , Arg { argIndex = VocabularyMapping,
                 argAbbr  = Just 'v',
                 argName  = Just "voc-map",
                 argData  = argDataRequired "mapping" ArgtypeString,
                 argDesc  = "Chord label comparison method (vocabulary mapping)"
               }      
         , Arg { argIndex = FileFormat,
                 argAbbr  = Just 'F',
                 argName  = Just "format",
                 argData  = argDataOptional "format" ArgtypeString,
                 argDesc  = "The kind of input format (json|lab)"
               } 
         , Arg { argIndex = ErrorStream,
                 argAbbr  = Just 'e',
                 argName  = Just "error",
                 argData  = argDataOptional "format" ArgtypeString,
                 argDesc  = "prints the error messages to a file or stderr"
               } 
         ] 

main :: IO ()
main = do arg <- parseArgsIO ArgsComplete myArgs
          let c  = pCollection arg
              y  = pYear arg
              
          mh <- pErrStr arg
          
          case fileOrDir arg of
            Left  f ->    (pEvalFuncFile arg) mh f 
            Right d -> do t <- pTeam arg d y c
                          (pEvalFuncDir arg) mh t d y c 

printReturn :: Show a => a -> IO (a)
printReturn a = print a >> return a
                          
-- | Checks for either a directory or file argument, returns them in an Either
-- or throws an error otherwise
fileOrDir :: Args MirexArgs -> Either FilePath FilePath
fileOrDir arg = case ( getArg arg MirexFilepath, getArg arg MirexDir
                     , getArg arg Team         , getArg arg ID       ) of
   (Just _, Just _, _      ,_      ) 
      -> usageError arg "found both a directory and file"
   (Just f, _     , _      ,_      ) 
      -> Left  f
   (_     , Just d, _      ,Nothing) 
      -> Right d
   (_     , Just d, Just tm,Just i ) 
      -> Left $ toFileName d (pYear arg) (pCollection arg) tm i (pFormat arg)
   (_     , _     , _      , _     ) 
      -> usageError arg "No directory or file specified"

pVerb :: Args MirexArgs -> (a -> b) -> Maybe (a -> b)
pVerb arg f | gotArg arg Print = Just f
            | otherwise        = Nothing
      
pTeam :: Args MirexArgs -> FilePath -> Year -> Collection -> IO (Maybe Team)
pTeam arg dir y c = case getArg arg Team of 
                      Nothing -> return Nothing 
                      Just tm -> do let fp = dir </> show y </> show c </> tm
                                    e <- doesDirectoryExist fp
                                    if e then return . Just $ tm
                                         else error  ("FilePath " ++ fp 
                                                   ++ " does not exist")
            
pCollection :: Args MirexArgs -> Collection
pCollection arg = case toCollection $ getRequiredArg arg Collection of
                   (Just c , _) -> c
                   (Nothing, e) -> usageError arg e
                    
pYear :: Args MirexArgs -> Year
pYear arg = case toYear $ getRequiredArg arg MirexYear of
              (Just y , _) -> y
              (Nothing, e) -> usageError arg e

pEvalFuncFile :: Args MirexArgs 
             -> Maybe Handle ->  FilePath -> IO ()
pEvalFuncFile arg = 
  case (getRequiredArg arg VocabularyMapping, gotArg arg Print) of
    ("mirex2010", True ) -> evaluateMChordsVerb (printOverlapEval mirex2010) overlapRatio
    ("mirex2010", False) -> evaluateMChords (overlapEval mirex2010) overlapRatio
    ("majMin"   , True ) -> evaluateMChordsVerb (printOverlapEval majMinEq) overlapRatio
    ("majMin"   , False) -> evaluateMChords (overlapEval majMinEq) overlapRatio
    ("root"     , True ) -> evaluateMChordsVerb (printOverlapEval rootOnlyEq) overlapRatio
    ("root"     , False) -> evaluateMChords (overlapEval rootOnlyEq) overlapRatio
    ("bass"     , True ) -> evaluateMChordsVerb (printOverlapEval bassOnlyEq) overlapRatio
    ("bass"     , False) -> evaluateMChords (overlapEval bassOnlyEq) overlapRatio
    ("triad"    , True ) -> evaluateMChordsVerb (printOverlapEval triadEq) overlapRatio
    ("triad"    , False) -> evaluateMChords (overlapEval triadEq) overlapRatio
    ("mirex2013", True ) -> evaluateMChordsVerb (printOverlapEval chordClassEq) overlapRatioCCEval
    ("mirex2013", False) -> evaluateMChords (overlapEval chordClassEq) overlapRatioCCEval
    -- probably it's better to create another mode
    ("underSeg" , True ) -> evaluateMChordsVerb hamDistUnderSegVerb normHamDist
    ("underSeg" , False) -> evaluateMChords hamDistUnderSeg normHamDist
    ("overSeg"  , True ) -> evaluateMChordsVerb hamDistOverSegVerb normHamDist
    ("overSeg"  , False) -> evaluateMChords hamDistOverSeg normHamDist
    ("segment"  , True ) -> usageError arg "please use overSeg and underSeg"
    ("segment"  , False) -> evaluateMChords segmentEval normSegEval
    (m, _) -> usageError arg ("unrecognised vocabulary mapping: " ++ m)   

              
pEvalFuncDir :: Args MirexArgs 
             -> Maybe Handle -> Maybe Team -> FilePath -> Year -> Collection 
             -> IO ()
pEvalFuncDir arg = let r  = const . return $ () 
                   in case getRequiredArg arg VocabularyMapping of
  "mirex2010" -> evaluateMirex (overlapEval mirex2010) reportAvgWOR r (pVerb arg overlapRatio)
  "majMin"    -> evaluateMirex (overlapEval majMinEq) reportAvgWOR r (pVerb arg overlapRatio)
  "root"      -> evaluateMirex (overlapEval rootOnlyEq) reportAvgWOR r (pVerb arg overlapRatio)
  "bass"      -> evaluateMirex (overlapEval bassOnlyEq) reportAvgWOR r (pVerb arg overlapRatio)
  "triad"     -> evaluateMirex (overlapEval triadEq) reportAvgWOR r (pVerb arg overlapRatio)
  "mirex2013" -> evaluateMirex (overlapEval chordClassEq) reportMIREX13 r (pVerb arg overlapRatioCCEval)
  "segment"   -> evaluateMirex segmentEval reportSegment r (pVerb arg normSegEval)
  m -> usageError arg ("unrecognised vocabulary mapping: " ++ m)   

pFormat :: Args MirexArgs -> Format
pFormat arg = case toFormat $ getRequiredArg arg FileFormat of
               (Just f , _) -> f
               (Nothing, e) -> usageError arg e 

pErrStr :: Args MirexArgs -> IO (Maybe Handle)
pErrStr arg = case getArg arg ErrorStream of
                Just "stderr" -> return (Just stderr)
                Just "err"    -> return (Just stderr)
                Just fp       -> openFile fp WriteMode >>= return . Just
                Nothing       -> return Nothing
              
