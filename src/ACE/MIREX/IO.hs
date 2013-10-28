{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types #-}
module ACE.MIREX.IO  ( evaluateMChords
                     , evaluateMChordsVerb
                     , evaluateMirex
                     ) where

import ACE.Parsers.ChordJSON
import ACE.Parsers.ChordLab
import ACE.MIREX.Data
import ACE.MIREX.PreProcessing
import ACE.Evaluation

import HarmTrace.Base.Time   ( Timed (..) )
import HarmTrace.Base.Chord  ( ChordLabel )
import HarmTrace.Base.Parse  ( parseDataWithErrors, Parser )

import Control.Monad         ( when )
import Data.Maybe            ( isJust, fromJust )
import System.Directory      ( getDirectoryContents )
import System.FilePath       ( (</>) )
import System.IO             ( hPutStrLn, Handle )
import Control.Concurrent.ParallelIO.Global ( parallel )

--------------------------------------------------------------------------------
-- MIREX data IO
--------------------------------------------------------------------------------

evaluateMChords :: Show b => ([Timed RefLab] -> [Timed ChordLabel] -> a)
                   -- ^ a function that evaluates a song 
                -> (a -> b)
                   -- ^ a possible Handle for routing the error messages
                -> Maybe Handle
                   -- ^ a function post-processes a evaluation result
                -> FilePath
                   -- ^ the input file
                -> IO ()
evaluateMChords ef pp mh fp = 
  do mc <- readMChords mh fp
     putStr (show mc ++ ": ") 
     print . evaluate (\a b -> pp $ ef a b) $ mc

evaluateMChordsVerb :: Show b => ([Timed RefLab] -> [Timed ChordLabel] -> IO a)
                   -- ^ a function that evaluates a song 
                -> (a -> b)
                   -- ^ a possible Handle for routing the error messages
                -> Maybe Handle
                   -- ^ a function post-processes a evaluation result
                -> FilePath
                   -- ^ the input file
                -> IO ()
evaluateMChordsVerb ef pp mh fp = 
  do mc <- readMChords mh fp
     putStrLn (show mc ++ ": ") 
     evaluate (\a b -> ef a b >>= return . pp) mc >>= print
     
  
-- | Given an evaluation metric, a 
-- MIREX results base directory, a year, and a collection, we evaluate all
-- chord recognition results from that particular year and collection
evaluateMirex :: (Show b, Show c) => ([Timed RefLab] -> [Timed ChordLabel] -> a) 
                 -- ^ a function that evaluates a song 
              -> ([a] -> IO b)
                 -- ^ a function that aggregates the results of multiple songs
              -> Maybe (a -> c)
                 -- ^ a possible Handle for routing the error messages
              -> Maybe Handle
                 -- ^ a function post-processes an individual evaluation result
                 -- that will be printed to the user
              -> Maybe Team 
                 -- ^ evaluates a specific team only
                 --TODO probably we don't need Year and Collection here
              -> FilePath -> Year -> Collection -> IO ()
evaluateMirex ef af mpp mh mteam dir y c =
   do let baseDir = dir </> show y </> show c

          -- | Evaluates the submission of a single team
          -- doTeam :: Show c => Team -> IO c
          doTeam tm = 
            do putStrLn $ "Team " ++ tm
               getTeamFiles tm >>= parallel . map evaluateMChord >>= af

          -- | returns the files for one team
          getTeamFiles :: Team -> IO [(Team, FilePath)]
          getTeamFiles tm = getCurDirectoryContents (baseDir </> tm)
                          >>= return . map (\fp -> (tm, baseDir </> tm </> fp))

          -- Evaluates a single file
          -- evaluateMChord :: (Team, FilePath) -> IO a
          evaluateMChord (tm, fp) = 
            do mc <- readMChords mh fp 
               if tm == team mc
                  then do let r = evaluate ef mc
                          when (isJust mpp) . putStrLn 
                             $ show mc ++ " " ++ (show . fromJust mpp $ r)
                          return r
                  else error "evaluateMChord: teams don't match"

      tms <- getCurDirectoryContents baseDir 
      -- if mteam is set, we only only evaluate one team, 
      -- and otherwise everything else too
      let tms' = case mteam of
                   Just t  -> filter (t ==) tms
                   Nothing -> tms
      mapM_ doTeam tms'

-- | Reads a MIREX file and returns an 'MChords'
readMChords :: Maybe Handle -> FilePath -> IO MChords
readMChords mh fp = 
  do let (b,y,c,tm,i,f) = fromFileName fp 
     t <- readFile fp 
     
     case f of 
       JS  ->    printPPLog mh show (preProcess . parseChords (pChordJSON y c)) t
       LAB -> do let pGT :: Parser MChords
                     pGT = pGroundTruth (parseChords (pLabMChords tm i y c) t)
                 gt  <- readFile (toFileName b y c "Ground-truth" i f)
                 printPPLog mh show (preProcess . parseChords pGT) gt 

parseChords :: Parser MChords -> String -> MChords
parseChords pf txt = case parseDataWithErrors pf txt of
       (mc, []) -> mc
       (_ , er) -> error (-- "parsing file "  ++ fp ++ " yields the following " ++
                       "parse errors:\n" ++ concatMap (\e -> show e ++ "\n") er)


-- | Applies an evaluation function to an 'MChords' 
evaluate :: ([Timed RefLab] -> [Timed ChordLabel] -> a) -> MChords -> a
evaluate ef mc = case groundTruth mc of
  (Just gt) -> ef (makeGT gt) (chords mc)
  _   -> error "evaluate: I did not find a ground-truth and chord prediction"

--------------------------------------------------------------------------------
-- Unexported IO utils
--------------------------------------------------------------------------------

                        
printPPLog :: Maybe Handle -> ([PPLog] -> String) -> (a -> (b, [PPLog])) -> a -> IO b
printPPLog mh pp f a = do  let (r, logs) = f a
                           case mh of
                             Just h  -> hPutStrLn h (pp logs)
                             Nothing -> return ()
                           return r
                        

-- | Like 'getCurDirectoryContents', but filters the results for "." and ".."
getCurDirectoryContents :: FilePath -> IO [FilePath]
getCurDirectoryContents fp = 
  getDirectoryContents fp >>= return . filter (\x -> x /= "." && x /= "..") 

