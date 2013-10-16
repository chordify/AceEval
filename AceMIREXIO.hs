{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types #-}
module AceMIREXIO  ( evaluateMChords
                   , evaluateMChordsVerb
                   , evaluateMirex
                   ) where

import ChordJSON
import ChordLab
import AceMIREX
import Evaluation
import HarmTrace.Base.Time           ( Timed (..), BeatTime (..), BeatTime (..)
                                     , offset, onset, duration, pprint )
import HarmTrace.Base.Chord          ( ChordLabel, Chord (..) )
import HarmTrace.Base.Parse          ( parseDataSafe, parseDataWithErrors, pChord, Parser )

import Control.Monad                 ( when, zipWithM )
import Control.Monad.State           ( State, modify, runState )
import Data.Foldable                 ( foldrM )
import Data.List                     ( intercalate, genericLength, partition )
import Data.Maybe                    ( isJust, fromJust )
import System.Directory              ( getDirectoryContents )
import System.FilePath               ( (</>) )
import System.IO                     ( hPutStrLn, stderr )
import Control.Concurrent.ParallelIO.Global ( parallel )

--------------------------------------------------------------------------------
-- MIREX data IO
--------------------------------------------------------------------------------

evaluateMChords :: ([Timed RefLab] -> [Timed ChordLabel] -> a)
                   -- ^ a function that evaluates a song 
                -> (a -> Double)
                   -- ^ a function post-processes a evaluation result
                -> FilePath -> Year -> Collection -> IO ()
evaluateMChords ef pp fp y c = 
  do mc <- readMChords y c fp
     putStr (show mc ++ ": ") 
     putStrLn . show . evaluate (\a b -> pp $ ef a b) $ mc
 

evaluateMChordsVerb :: Show a => ([Timed RefLab] -> [Timed ChordLabel] -> IO a) 
                    -> FilePath -> Year -> Collection -> IO a
evaluateMChordsVerb ef fp y c = 
  do mc <- readMChords y c fp
     print mc
     r <- evaluate ef mc 
     print r
     return r
     
  
-- | Given an evaluation metric, a 
-- MIREX results base directory, a year, and a collection, we evaluate all
-- chord recognition results from that particular year and collection
evaluateMirex :: ([Timed RefLab] -> [Timed ChordLabel] -> a) 
                 -- ^ a function that evaluates a song 
              -> ([a] -> IO Double)
                 -- ^ a function that aggregates the results of multiple songs
              -> Maybe (a -> Double)
                 -- ^ a function post-processes an individual evaluation result
                 -- that will be printed to the user
              -> Maybe Team 
                 -- ^ evaluates a specific team only
              -> FilePath -> Year -> Collection -> IO [Double]
evaluateMirex ef af mpp mteam dir y c =
   do let baseDir = dir </> show y </> show c

          -- | Evaluates the submission of a single team
          doTeam :: Team -> IO Double
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
            do mc <- readMChords y c fp 
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
      mapM doTeam tms'

-- | Reads a MIREX file and returns an 'MChords'
readMChords :: Year -> Collection -> FilePath -> IO MChords
readMChords y c fp = 
  do let (b,y,c,tm,i,f) = fromFileName fp 
     txt <- readFile fp 
     
     case f of 
       JS  ->    postProcess . parseChords (pChordJSON y c) $ txt
       LAB -> do gt  <- readFile (toFileName b y c "Ground-truth" i f)
                 postProcess . parseChords (pGroundTruth 
                           (parseChords (pLabMChords tm i y c) txt)) $ gt 
                        
parseChords :: Parser MChords -> String -> MChords
parseChords pf txt = case parseDataWithErrors pf txt of
       (mc, []) -> mc
       (_ , er) -> error (-- "parsing file "  ++ fp ++ " yields the following " ++
                       "parse errors:\n" ++ concatMap (\e -> show e ++ "\n") er)

data Edit = Fill | Zero ChordLabel

instance Show Edit where
  show Fill     = "hole in "
  show (Zero c) = "Zero length segment for " ++ show c ++ " "

data EditLog = EditLog Edit Collection Year String Int Double Double 
                       
instance Show EditLog where
  show (EditLog e c y t i on off) = 
    (show e ++ intercalate " " [show c, show y, t, show i] 
            ++ ": " ++ show off ++ " - " ++ show on)
                                
fillFromMChords :: MChords -> Timed ChordLabel -> EditLog
fillFromMChords mc c = EditLog Fill (collection mc) (year mc) (team mc) 
                               (songID mc) (onset c) (offset c)

zeroFromMChords :: MChords -> Timed ChordLabel -> EditLog
zeroFromMChords mc c = EditLog (Zero (getData c)) (collection mc) (year mc) 
                               (team mc) (songID mc) (onset c) (offset c)                               
                       
postProcess :: MChords -> IO MChords
postProcess mc = do c  <- process . chords $ mc
                    gt <- maybeIO process (groundTruth mc)
                    
                    return mc { chords = c, groundTruth = gt } where
  
  maybeIO :: (a -> IO b) -> Maybe a -> IO (Maybe b)
  maybeIO f ma = case ma of Just a  -> f a >>= return . Just
                            Nothing -> return Nothing
  
  process :: [Timed ChordLabel] -> IO [Timed ChordLabel]
  process cs = do let (cs', es) = runState (fill cs >>= filterZeroLen) []
                  mapM_ (putErrStrLn . show) es
                  return cs'
  
  fill :: [Timed ChordLabel] -> State [EditLog] [Timed ChordLabel]
  fill cs = do foldrM step [] cs >>= return where

    step :: Timed ChordLabel ->[Timed ChordLabel] -> State [EditLog] [Timed ChordLabel]
    step a []     = return [a]
    step a (b:ts) 
      | off == on =                   return (a        : b : ts)
      | otherwise = modify (log :) >> return (a : hole : b : ts)
                       
           where off  = offset a
                 on   = onset  b 
                 hole = Timed NoChord [Time off, Time on]
                 log  = fillFromMChords mc hole
                 
  filterZeroLen :: [Timed ChordLabel] -> State [EditLog] [Timed ChordLabel] 
  filterZeroLen td = do let (zero, good) = partition (\x -> duration x == 0) td
                        modify ((map (zeroFromMChords mc) zero) ++)
                        return good 
                       
-- | Applies an evaluation function to an 'MChords' 
evaluate :: ([Timed RefLab] -> [Timed ChordLabel] -> a) -> MChords -> a
evaluate ef mc = case groundTruth mc of
  (Just gt) -> ef (makeGT gt) (chords mc)
  _   -> error "evaluate: I did not find a ground-truth and chord prediction"

--------------------------------------------------------------------------------
-- Unexported IO utils
--------------------------------------------------------------------------------

-- | Like 'getCurDirectoryContents', but filters the results for "." and ".."
getCurDirectoryContents :: FilePath -> IO [FilePath]
getCurDirectoryContents fp = 
  getDirectoryContents fp >>= return . filter (\x -> x /= "." && x /= "..") 
              
putErrStrLn :: String -> IO ()
putErrStrLn s = hPutStrLn stderr s

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

testA = ["C:maj","C:min","C:dim","C:aug","C:maj","C#:maj","C:dim7","C:maj"
        ,"C:maj","C:min","C:maj","C:dim","C:sus2","C:min","C:min"]
testB = ["C:maj","C:min","C:dim","C:aug","C:min","Db:maj","C:(1,#2,#4,6)"
        ,"C:(1,#2,#4,6)","C:(1,b3,5)","C:(1,#2,5)","C:maj7","C:sus2","C:sus4"
        ,"C:dim","C:aug"]
     
testEq :: (ChordLabel -> ChordLabel -> EqIgnore) -> [String] -> [String] 
       -> IO [EqIgnore]
testEq eq a b = zipWithM eqIO a b where

  eqIO :: String -> String -> IO EqIgnore
  eqIO x y = do let r = eq (parseDataSafe pChord x) (parseDataSafe pChord y)
                putStrLn (x ++ '\t' : y ++ '\t' : show r)
                return r
