{-# OPTIONS_GHC -Wall #-}
module Main where

-- other libraries
import System.Console.ParseArgs
import ChordJSON
import AceMIREX
import AceMIREXIO
import Evaluation
import HarmTrace.Base.Chord       ( ChordLabel )
import HarmTrace.Base.Time        ( Timed )
import Control.Monad              ( void )
import System.FilePath            ( (</>), (<.>) )
import System.Directory           ( doesDirectoryExist )
import Text.Printf                ( printf )
import Data.Maybe                 ( isJust )

data MirexArgs = MirexDir | MirexFilepath | MirexYear | Print | Team | ID
               | Collection | GroundTruthDir | VocabularyMapping | FileFormat
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
                 argAbbr  = Just 'f',
                 argName  = Just "format",
                 argData  = argDataOptional "format" ArgtypeString,
                 argDesc  = "The kind of input format (json|lab)"
               } 
         ] 

main :: IO ()
main = do arg <- parseArgsIO ArgsComplete myArgs
          let c  = pCollection arg
              y  = pYear arg
              p  = pVerb arg pp
              vm = pVocMap arg 
              ef = overlapEval vm 
              pp = overlapRatio
              
              pEq :: [Timed RefLab] -> [Timed ChordLabel] -> IO Double
              pEq a b = printOverlapEval vm a b >>= return . pp
              
          case fileOrDir arg of
            Left  f -> if isJust p then void $ evaluateMChordsVerb pEq f y c
                                   else evaluateMChords ef pp f y c
            Right d -> do t <- pTeam arg d y c
                          void $ evaluateMirex ef weightOverlapRatio p t d y c 

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

pVerb :: Args MirexArgs -> (a -> Double) -> Maybe (a -> Double)
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
pCollection arg = case toCollection $ getRequiredArg arg MirexYear of
                   (Just c , _) -> c
                   (Nothing, e) -> usageError arg e
                    
pYear :: Args MirexArgs -> Year
pYear arg = case toYear $ getRequiredArg arg MirexYear of
              (Just y , _) -> y
              (Nothing, e) -> usageError arg e

pVocMap :: Args MirexArgs -> RefLab -> ChordLabel -> EqIgnore
pVocMap arg = case getRequiredArg arg VocabularyMapping of
                "mirex2010" -> mirex2010
                "majMin"    -> majMinEq
                "root"      -> rootOnlyEq
                m -> usageError arg ("unrecognised vocabulary mapping: " ++ m)

pFormat :: Args MirexArgs -> Format
pFormat arg = case toFormat $ getRequiredArg arg MirexYear of
               (Just f , _) -> f
               (Nothing, e) -> usageError arg e 


              
