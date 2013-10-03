{-# OPTIONS_GHC -Wall #-}
module Main where

-- other libraries
import System.Console.ParseArgs
import ChordJSON
import Evaluation
import HarmTrace.Base.Chord       ( ChordLabel )
import HarmTrace.Base.Time        ( Timed )
import Control.Monad              ( void )
import System.FilePath            ( (</>), (<.>) )
import System.Directory           ( doesDirectoryExist )
import Text.Printf                ( printf )
import Data.Maybe                 ( isJust )

data MirexArgs = MirexDir | MirexFilepath | MirexYear | Print | Team | ID
               | Collection | GroundTruthDir | VocabularyMapping 
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
      -> Left $ toFileName d (pYear arg) (pCollection arg) tm i
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
pCollection arg = case getRequiredArg arg Collection of
                    "bb"        -> Billboard
                    "bs"        -> Beatles
                    "Billboard" -> Billboard
                    "Beatles"   -> Beatles
                    m -> usageError arg ("unrecognised collection: " ++ m)
                    
pYear :: Args MirexArgs -> Year
pYear arg = case getRequiredArg arg MirexYear of
              "2010" -> Y2010
              "2011" -> Y2011
              "2012" -> Y2012
              m -> usageError arg ("unrecognised year: " ++ m)

pVocMap :: Args MirexArgs -> RefLab -> ChordLabel -> EqIgnore
pVocMap arg = case getRequiredArg arg VocabularyMapping of
                "mirex2010" -> mirex2010
                "majMin"    -> majMinEq
                "root"      -> rootOnlyEq
                m -> usageError arg ("unrecognised vocabulary mapping: " ++ m)

-- data VocMap = Mirex2010 | Root | MajMin | Seventh | MajMinInv | SevInv

-- | Returns the vocabulary mapping function belonging to a certain 'VocMap'
-- vocMapFunc :: VocMap -> 
-- vocMapFunc Mirex2010 = mirex2010
-- vocMapFunc MajMin    = majMinEq
-- vocMapFunc Root      = rootOnlyEq
              
toFileName :: FilePath -> Year -> Collection -> Team -> Int -> FilePath
toFileName dir y c t i = dir </> show y </> show c </> t </> toID where

  toID :: String
  toID = case c of
           Beatles   -> printf "chordmrx09000%03d.js" i
           Billboard -> show i <.> "js"
              
