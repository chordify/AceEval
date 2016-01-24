{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types #-}
module ACE.MIREX.IO  where
-- ( evaluateMChords
--                     , evaluateMChordsVerb
--                     , evaluateMirex
--                     , fusionMirex
--                     ) where

import ACE.Parsers.ChordJSON
import ACE.Parsers.ChordLab
import ACE.MIREX.Data
import ACE.MIREX.PreProcessing
import ACE.Evaluation
import ACE.Evaluation.ChordClass
import HarmTrace.Base.Parse.ChordParser
import HarmTrace.Base.Parse.General 

import HarmTrace.Base.Time  
 -- ( Timed (..) )
import HarmTrace.Base.Chord  ( ChordLabel )
import HarmTrace.Base.Parse  
--( parseDataWithErrors, Parser, parseBillboardSafe )

import Control.Monad         ( when )
import Data.Maybe            ( isJust, fromJust )
import Data.List             ( intercalate )
import System.Directory      ( getDirectoryContents )
import System.FilePath       ( (</>) )
import System.IO             ( hPutStrLn, Handle )
import Control.Concurrent.ParallelIO.Global ( parallel )

import ACE.Parsers.ChordLab
--import HarmTrace.Base.Parse  ( parseDataWithErrors, Parser )
import HarmTrace.Base.Chord.PitchClass
import Data.Function (on)
import Data.List (groupBy, sort, sortBy, nub)
import Data.Ord (comparing)

import Fusion.Calc (listHandle, listHandleGeneric)

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
                   -- ^ a function post-processes a evaluation result
                -> Maybe Handle
                   -- ^ a possible Handle for routing the error messages
                -> FilePath
                   -- ^ the input file
                -> IO ()
evaluateMChordsVerb ef pp mh fp = 
  do mc <- readMChords mh fp
     putStrLn (show mc ++ ": ") 
     evaluate (\a b -> ef a b >>= return . pp) mc >>= print
     
  
-- | Given an evaluation function that compares two chord sequences, a function
-- that aggregates the results per team, a function that aggregates the results
-- for all teams, and a directory, evaluateMirex performs the evaluation. 
-- Optionally... 
evaluateMirex :: (Show b, Show c) => ([Timed RefLab] -> [Timed ChordLabel] -> a) 
                 -- ^ a function that evaluates a song 
              -> ([a] -> IO b)
                 -- ^ a function that aggregates the results of multiple songs
              -> ([b] -> IO ())   
                 -- ^ a function that aggregates the results of multiple teams
              -> Maybe (Team -> String)
                 -- ^ a function that specifies how the team name should be
                 -- printed
              -> Maybe [a -> c]
                 -- ^ a function post-processes an individual evaluation result
                 -- that will be printed to the user providing verbose feedback             
              -> Maybe Handle
                 -- ^ a possible Handle for routing the error messages
              -> Maybe Team 
                 -- ^ evaluates a specific team only, if set
                 --TODO probably we don't need Year and Collection here
              -> FilePath -> IO ()
evaluateMirex ef af atf mtp mpp mh mteam dir =
   do let -- | Evaluates the submission of a single team
          -- doTeam :: Show c => Team -> IO c
          doTeam tm = 
            do when (isJust mtp) . putStr . (fromJust mtp) $ tm
               tr <- getTeamFiles tm >>= parallel . map evaluateMChord 
               af $! tr

          -- | returns the files for one team
          getTeamFiles :: Team -> IO [(Team, FilePath, FilePath)]
          getTeamFiles tm = getCurDirectoryContents (dir </> tm)
                        >>= return . map (\fp -> (tm, dir </> tm, fp)) . reverse

          -- Evaluates a single file
          -- evaluateMChord :: (Team, FilePath) -> IO a
          evaluateMChord (tm, dir, fp) = 
            do mc <- readMChords mh (dir </> fp) 
               if tm == team mc
                  then let r    = evaluate ef mc
                           sf p = show . p $ r
                       in case mpp of
                           Just l  -> do putStrLn . intercalate "," $ (fp : map sf l)
                                         return r
                           Nothing -> r `seq` return r                                
                  else error "evaluateMChord: teams don't match"

      tms <- getCurDirectoryContents dir 
      -- if mteam is set, we only only evaluate one team, 
      -- and otherwise we only ignore the "Ground-Truth" directory
      let tms' = case mteam of
                   Just t  -> filter (t ==) tms
                   Nothing -> filter ("Ground-truth" /=) tms
      ar <- mapM doTeam tms' -- all results 
      atf $! ar

-- | fusionMirex... 
fusionMirex :: (Show b) => ([MChords] -> IO b)
                 -- at: ^ a function that aggregates the results of multiple songs
              -> ([b] -> IO ())   
                 -- atf: ^ a function that aggregates the results of multiple teams
                 -- This should be fusion?
              -> Maybe (Team -> String)
                 -- mtp: ^ a function that specifies how the team name should be
                 -- printed             
              -> Maybe Team 
                 -- mteam: ^ evaluates a specific team only, if set
              -> FilePath -> IO ()
fusionMirex af atf mtp mteam dir =
   do let -- | Evaluates the submission of a single team
          -- doTeam :: Show c => Team -> IO c
          doTeam tm = 
            do when (isJust mtp) . putStr . (fromJust mtp) $ tm
               tr <- getTeamFiles tm >>= parallel . map evaluateMChord 
               --af $! tr
               return (tr)

          -- | returns the files for one team
          getTeamFiles :: Team -> IO [(Team, FilePath, FilePath)]
          getTeamFiles tm = getCurDirectoryContents (dir </> tm)
                        >>= return . map (\fp -> (tm, dir </> tm, fp)) . reverse

          -- Evaluates a single file
          -- evaluateMChord :: (Team, FilePath) -> IO a
          evaluateMChord (tm, dir, fp) = 
            do mc <- toMChordsEH (dir </> fp) 
               if tm == team mc
                  then return mc                                
                  else error "evaluateMChord: teams don't match"

      tms <- getCurDirectoryContents dir 
      -- if mteam is set, we only only evaluate one team, 
      -- and otherwise we only ignore the "Ground-Truth" directory
      let tms' = case mteam of
                   Just t  -> filter (t ==) tms
                   Nothing -> filter ("Ground-truth" /=) tms
      ar <- mapM doTeam tms' -- all results 
      -- group from all teams by songID
      let gar = groupByIDs . concat $ ar
      -- align per songID, i.e. sample every n seconds, and fuse
      fusedAllR <- mapM (fuseMChords 5 (0.1) toCCRoots fromStringRootPCs) gar
      --fusedAllMM <- mapM (fuseMChords 5 (0.1) toCCMajMins toChordClasses) gar
      -- now write them all to files 
      --putStrLn . show . head $ fusedAllR
      return ()

filterMChordsID :: SongID -> [[MChords]] -> [[MChords]]
filterMChordsID sid = filter (\mcc -> ((head . map songID)  mcc)  == sid)

-- | plotFusion... 
plotFusion :: (Show b) => ([MChords] -> IO b)
                 -- at: ^ a function that aggregates the results of multiple songs
              -> ([b] -> IO ())   
                 -- atf: ^ a function that aggregates the results of multiple teams
                 -- This should be fusion?
              -> Maybe (Team -> String)
                 -- mtp: ^ a function that specifies how the team name should be
                 -- printed             
              -> Maybe SongID 
                 -- mteam: ^ evaluates a specific SongID only, if set
              -> FilePath -> IO ()
plotFusion af atf mtp msong dir =
   do let -- | Evaluates the submission of a single team
          -- doTeam :: Show c => Team -> IO c
          doTeam tm = 
            do when (isJust mtp) . putStr . (fromJust mtp) $ tm
               tr <- getTeamFiles tm >>= parallel . map evaluateMChord 
               --af $! tr
               return (tr)

          -- | returns the files for one team
          getTeamFiles :: Team -> IO [(Team, FilePath, FilePath)]
          getTeamFiles tm = getCurDirectoryContents (dir </> tm)
                        >>= return . map (\fp -> (tm, dir </> tm, fp)) . reverse

          -- Evaluates a single file
          evaluateMChord :: (Team, FilePath, FilePath) -> IO MChords
          evaluateMChord (tm, dir, fp) = 
            do mc <- readMChords Nothing (dir </> fp) 
               if tm == team mc
                  then return mc                                
                  else error "evaluateMChord: teams don't match"

      tms <- getCurDirectoryContents dir 
      ar <- mapM doTeam tms -- all results 
      -- group from all teams by songID
      let arS = groupByIDs . concat $ ar
      -- if msong is set, we only only evaluate one team, 
      -- and otherwise we only ignore the "Ground-Truth" directory
      let arS' = case msong of
                   Just s  -> filterMChordsID s arS
                   Nothing -> arS
      --let garPP = map (map $ fst . preProcess) . groupByIDs . concat $ ar
      let garS = map (sampleMChordsM 0.1) arS'
      -- align per songID, i.e. sample every n seconds, and fuse
      fusedAllR <- mapM (fuseMChordsM 5 (0.1) mchordsToInt intPCtoChordLabel) garS 
      let mc2 = map (fst . preProcess) fusedAllR
      let bls = map (evaluateFusion (overlapEval rootOnlyEq)) mc2
      putStrLn . show $ bls
      return ()

-- 
-- plotFusion (reportFusion) (const . return $ ()) (Just tpf) (Just 1301) "/Users/hvkoops/repos/mirexfusion/MirexFusion/algorithmic-output/2013/BillBoard2013"
-- plotFusion (reportFusion) (const . return $ ()) (Just tpf) Nothing "/Users/hvkoops/repos/mirexfusion/MirexFusion/algorithmic-output/2013/BillBoard2013"

-- This should output ID TEAM1, TEAM2, ... TEAMN, FUSION accuracies per songid
evaluateFusion :: ([Timed RefLab] -> [Timed ChordLabel] -> [Timed EqIgnore]) -> MChords -> Double
evaluateFusion ef = overlapRatio . evaluate ef 

sampletoChordClass :: NumData -> [MChords] -> [[ChordClass]]
sampletoChordClass spl = ((map.map) toChordClass) . (sampleMChords spl)

-- Int is fusioniterations, NumData is sample frequency, 
-- cfront is a conversion function (e.g. to roots) for fusion
-- cback is a conversion function (e.g. to roots) to fuse a chord class
fuseMChordsM :: (Show a, Ord a) => Int -> NumData -> (MChords -> [a]) -> (a -> ChordLabel) -> [MChords] -> IO (MChords)
fuseMChordsM n spl cfront cback mc = do 
  -- convert from ChordClass with cfront:
  let newrep = map cfront mc
  -- fuse the converted chords
  fusedr <- listHandleGeneric n newrep "testfuse"
  -- convert back to [Chordlabel]:
  let fusedrTC = map cback fusedr
  -- reattach the timestamps 
  let fusedHT = attachTime spl fusedrTC

  -- make new MChords:
  let newmc = insertNewChords (mc!!0) fusedHT
  return (newrep `seq` fusedr `seq` fusedrTC `seq` fusedHT `seq` newmc)

insertNewChords :: MChords -> [Timed ChordLabel] -> MChords
insertNewChords (MChords c y t s ch g) newch = (MChords c y "FUSION" s newch g)

-- Int is fusioniterations, NumData is sample frequency, 
-- cfront is a conversion function (e.g. to roots) to fuse a chord class
-- cback converts Strings to ChordClasses
fuseMChords :: (Show a) => Int -> NumData -> ([ChordClass] -> [a]) -> ([String] -> [ChordClass]) -> [MChords] -> IO [ChordClass]
fuseMChords n spl cfront cback mc = do 
  -- sample the MChords
  let sMChords = sampletoChordClass spl mc
  -- convert from ChordClass with cfront:
  let newrep = map cfront sMChords
  let srcs = (map.map) show $ newrep
  putStrLn . show . head $ srcs
  -- fuse the converted chords
  fusedr <- listHandle n srcs "testfuse"
  -- covert back to ChordClass with cback:
  let fusedrCC = cback fusedr
  -- reattach the timestamps 
  -- let fusedHT = attachTime spl fusedH
  return (fusedrCC)

---- majmin:
--let mms = map toCCMajMins sMChords
--let srcsmm = (map.map) show $ mms
--fusedmm <- listHandle n srcsr "testfuse"
--let fusedmmCC = map (toChordClass . parseData pChord) fusedmm

attachTime :: NumData -> [a] -> [Timed a]
attachTime spl l = zipWith3 timed l [0.0, spl ..] [spl, (spl+spl) ..]

attachTimeUntil :: NumData -> NumData -> [a] -> [Timed a]
attachTimeUntil spl end l = zipWith3 timed l [0.0, spl .. end] [spl, (spl+spl) .. end]

-- | Given a [MChords], sample the chord labels at every [10 ms]
--  !!!!! -> shouldnt be longest but length of ground truth
sampleMChordsM :: NumData -> [MChords] -> [MChords]
sampleMChordsM spl mcs = map (updateSampledMC spl longest) mcs where 
  sampledlist = map (sampleWith spl . chords) $ mcs 
  longest = maximum . (map length) $ sampledlist

updateSampledMC :: NumData -> Int -> MChords -> MChords
updateSampledMC spl longest (MChords c y t s ch g) = (MChords c y t s newch g) where
  newch = sampleWithLengthT spl longest ch

-- | Given a [MChords], sample the chord labels at every [10 ms]
sampleMChords :: NumData -> [MChords] -> [[ChordLabel]]
sampleMChords spl mc = map ((sampleWithLength spl longest) . chords) mc where
  sampledlist = map (sampleWith spl . chords) $ mc 
  longest = maximum . (map length) $ sampledlist

-- This should start at 0.00 and always end at the longest in the list
-- | Given a chord annotation sample the chord label at every 10 ms
-- like sample, but takes a sample rate (seconds :: Float) as argument
sampleWith :: NumData -> [Timed a] -> [a]
sampleWith rate = sampleAt [0.00, rate .. ] 

--sampleUntil :: NumData -> NumData -> [Timed a] -> [Timed a]
--sampleUntil rate end l = attachTime rate $ (newhead++newtail) where
--  newhead = sampleAt [0.00, rate .. ] l
--  lastC   = head . reverse $ newhead
--  newtail = take (n-(length newhead)) (repeat lastC)

sampleWithLengthT :: NumData -> Int -> [Timed a] -> [Timed a]
sampleWithLengthT rate n l = attachTime rate $ (newhead++newtail) where
  newhead = sampleAt [0.00, rate .. ] l
  lastC   = head . reverse $ newhead
  newtail = take (n-(length newhead)) (repeat lastC)

sampleWithLength :: NumData -> Int -> [Timed a] -> [a]
sampleWithLength rate n l = newhead++newtail where
  newhead = sampleAt [0.00, rate .. ] l
  lastC   = head . reverse $ newhead
  newtail = take (n-(length newhead)) (repeat lastC)
        
-- samples at specific points in time, specified in a list
sampleAt :: [NumData] -> [Timed a] -> [a]
sampleAt  _  [] = [] -- below, will never occur
sampleAt []  _  = error "Harmtrace.Audio.Evaluation: No sampling grid specified" 
sampleAt (t:ts) (c:cs)
  | t <= offset c = getData c : sampleAt ts (c:cs)
  | otherwise     = sampleAt (t:ts) cs   

smallestDuration :: [Timed ChordLabel] -> Double
smallestDuration = (!!0) . sort . map duration

groupByIDs :: [MChords] -> [[MChords]]
groupByIDs = groupBy ((==) `on` songID) . sortBy (comparing songID)

allIDS :: [MChords] -> [Int]
allIDS mc = map songID mc

-- return MChords from filepath
toMChords :: FilePath -> IO (MChords)
toMChords fp = do
  fc <- readFile fp
  let (b,y,c,tm,i,f) = fromFileName fp
      pGT :: Parser MChords
      pGT = pGroundTruth (parseChords (pLabMChords tm i y c) fc)
      mc = parseChords pGT fc
  return (mc)

-- return MChords from filepath, ignore pitch spelling
toMChordsEH :: FilePath -> IO (MChords)
toMChordsEH fp = do
  MChords c y t s ch g <- toMChords fp
  let newch = ignorePSTimed ch
  let ehmc = MChords c y t s newch g
  return (ehmc)

-- ignore the pitch spelling of [Timed ChordLabel] 
ignorePSTimed :: [Timed ChordLabel] -> [Timed ChordLabel] 
ignorePSTimed l = map ignoreTimedPitchSpelling l where
  ignoreTimedPitchSpelling :: Timed ChordLabel -> Timed ChordLabel
  ignoreTimedPitchSpelling (Timed c t) = Timed (ignorePitchSpelling c) t

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
       (_ , e ) -> error ("parse errors:\n" ++(intercalate "\n" . map show $ e))


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
  getDirectoryContents fp >>= return . filter (\x -> x /= "." && x /= ".." && x/= ".DS_Store") 

--------------------------------------------------------------------------------
-- Parse Strings to HarmTrace data. This should be removed later
--------------------------------------------------------------------------------


-- | Parses a Chordify chord annotation
parseBillboard :: String -> ([(NumData, NumData, ChordLabel)], [Error LineColPos])
parseBillboard = parseDataWithErrors parseAnnotationData

parseBillboardSafe :: String -> [(NumData, NumData, ChordLabel)]
parseBillboardSafe = parseDataSafe parseAnnotationData

-- | Parses a chord annotation.
parseAnnotationData :: Parser [(NumData, NumData, ChordLabel)]
parseAnnotationData =  pListSep_ng pLineEnd pChordSegment
                    <* (pLineEnd `opt` "\n")

-- | Parses the onset, offset and chordlabel on one line
pChordSegment :: Parser (NumData, NumData, ChordLabel)
pChordSegment = timedData' <$> pNumData <* (pSym '\t' <|> pSym ' ' <|>  (pSym ' ' <* pSym '\t') <|>  (pSym '\t' <* pSym ' '))
                           <*> pNumData <* (pSym '\t' <|> pSym ' ' <|>  (pSym ' ' <* pSym '\t') <|>  (pSym '\t' <* pSym ' '))
                           <*> pChord   where

  -- | convenient constructor for a 'Timed'
  timedData' :: NumData -> NumData -> ChordLabel -> (NumData, NumData, ChordLabel)
  timedData' onn off c = (onn, off, c)

  -- TODO : it would not hurt to move the functions below to HarmTrace-Base
-- because they are very general                           
-- | Parses a 'Beat'.
pBeat :: Parser Beat
pBeat =   One   <$ pSym '1'
      <|> Two   <$ pSym '2'
      <|> Three <$ pSym '3'
      <|> Four  <$ pSym '4'
      <?> "Beat"
-- pBeat = toBeat <$> pDigit where

  -- toBeat :: Char -> Beat
  -- toBeat '1' = One
  -- toBeat '2' = Two
  -- toBeat '3' = Three
  -- toBeat '4' = Four
  -- toBeat _   = NoBeat
  -- -- toBeat b   = error ("ChordSeqParser: unknown beat " ++ show b)
                           
-- Parses a time stamp
pNumData :: Parser NumData
pNumData = pDoubleRaw