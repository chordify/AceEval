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
import HarmTrace.Base.Chord 

import HarmTrace.Base.Time  
import HarmTrace.Base.Chord  ( ChordLabel )
import HarmTrace.Base.Parse  

import Control.Monad         ( when )
import Data.Maybe            ( isJust, fromJust )
import Data.List             ( intercalate )
import System.Directory      ( getDirectoryContents )
import System.FilePath       ( (</>) )
import System.IO             ( hPutStrLn, Handle )
import Control.Concurrent.ParallelIO.Global ( parallel )

import ACE.Parsers.ChordLab
import HarmTrace.Base.Chord.PitchClass
import Data.Function         (on)
import Data.List             (groupBy, sort, sortBy, nub, transpose)
import Data.Ord              (comparing)

import Fusion.Calc           (listHandle,  listHandleGenericQuietDP, pickNU,
                              listMVGenericQuiet, listRandomGenericQuiet,
                              listHandleGenericQuietDSub, 
                              listHandleGenericQuietD, listHandleGenericVerbose)

type CCEvalFunction = ([Timed RefLab] -> [Timed ChordLabel] -> [Timed (CCEval EqIgnore)])
type SongResults    = (SongID,[(Team,Double)])
type Results        = [SongResults]
--type Strategy       = ([a] -> [[a]] -> IO ([a]))

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

filterMChordsID :: SongID -> [[MChords]] -> [[MChords]]
filterMChordsID sid = filter (\mcc -> ((head . map songID)  mcc)  == sid)

fusionMirex :: (Ord a, Show a) 
              => Maybe SongID 
              -- msong: ^ evaluates a specific SongID only, if set
              -> ([ChordLabel] -> [a])
              -- ^ converts an MChords to a new representation, e.g. roots
              -> (a -> ChordLabel)
              -- ^ converts new representation back to MChords
              -> CCEvalFunction
              -- a corresponding eval function, e.g. (overlapEval rootOnlyEq)
              -> (CCEval Double -> Double)
              -- what to evaluate. e.g. eMajMin
              -> (RefLab -> ChordLabel -> EqIgnore)
              -> String 
              -- ^ same but string
              -> FilePath 
              -- ^ Path to all files
              -> NumData
              -- ^ Sampling frequency
              -> IO ()
fusionMirex msong cfront cback feval ev eveq sev dir s =
   do putStrLn . show $ dir
      let mtp t  = "Parsing submissions from team: " ++ show t ++ "\n"
          -- | Evaluates the submission of a single team
          doTeam tm = 
            do putStr . mtp $ tm
               tr <- getTeamFiles tm >>= parallel . map evaluateMChord 
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
      
      -- without GT
      tms <- getCurDirectoryContents dir 
      ar <- mapM doTeam tms -- all results 
      let arNoGT = filter (\mc -> team mc /= "Ground-truth") . concat $ ar
      -- group from all teams by songID
      let arS   = groupByIDs arNoGT
      -- if msong is set, we only only evaluate one team, 
      -- and otherwise we only ignore the "Ground-Truth" directory
      let arS'  = case msong of
                   Just s  -> filterMChordsID s arS
                   Nothing -> arS
      -- align per songID, i.e. sample every n seconds, and fuse
      let garS  = map (sampleMChordsM s) arS'
    
      -- with GT for glass ceiling
      let arSGT   = groupByIDs . concat $ ar
      -- if msong is set, we only only evaluate one team
      let arSGT'  = case msong of
                   Just s  -> filterMChordsID s arSGT
                   Nothing -> arSGT
      -- align per songID, i.e. sample every n seconds, and fuse
      let garSGT  = map (sampleMChordsM s) arSGT'

      (ceilings, fusedAllR, mvAllR, rAllR) <- combineAll cfront cback feval ev eveq s garS garSGT
      
      let garSPP = (map.map) (fst . preProcess) garS

      let mcF    = map (fst . preProcess) fusedAllR
      
      let wcsr = weightOverlapRatio . map (evaluate (overlapEval majMinEq)) $ fusedAllR
      putStrLn ("wcsr = " ++ (show wcsr))
      
      let mcMV   = map (fst . preProcess) mvAllR
      let mcR    = map (fst . preProcess) rAllR
      let mcC    = map (fst . preProcess) ceilings
      
      let both    = zipWith (++) garSPP $ map (:[]) mcC
      let both1   = zipWith (++) both   $ map (:[]) mcR
      let both2   = zipWith (++) both1  $ map (:[]) mcMV
      let both3   = zipWith (++) both2  $ map (:[]) mcF
      
      blsf <- parallel . map (evaluateFusionSong feval ev) $ both3
      let coll   = show . collection . head $ mcF
      writeCSV (coll++"_"++sev++".csv") blsf
      return ()

combineAll :: (Ord a, Show a) => ([ChordLabel] -> [a])
              -- ^ converts an MChords to a new representation, e.g. roots
              -> (a -> ChordLabel)
              -- ^ converts new representation back to MChords
              -> CCEvalFunction
              -- a corresponding eval function, e.g. (overlapEval rootOnlyEq)
              -> (CCEval Double -> Double)
              -- what to evaluate. e.g. eMajMin
              -> (RefLab -> ChordLabel -> EqIgnore)
              -- ^ same but string
              -> NumData
              -- ^ Sampling frequency
              -> [[MChords]]
              -> [[MChords]]
              -> IO ([MChords], [MChords], [MChords], [MChords])
combineAll cfront cback feval ev eveq s garS garSGT = do
    -- glass ceiling 
  ceilings  <- mapM (fusionBaseLine ev eveq feval) $ garSGT
  -- data fusion
  fusedAllR <- mapM (combineChordsM "FUSION" s cfront cback (listHandleGenericQuietD 5)) garSGT
  -- majority vote
  mvAllR    <- mapM (combineChordsM "MVOTE"  s cfront cback (listMVGenericQuiet      4)) garS 
  -- random picking
  rAllR     <- mapM (combineChordsM "RANDOM" s cfront cback (listRandomGenericQuiet  4)) garS 
  return (ceilings, fusedAllR, mvAllR, rAllR)

combineAllRand :: (Ord a, Show a) => ([ChordLabel] -> [a])
              -- ^ converts an MChords to a new representation, e.g. roots
              -> (a -> ChordLabel)
              -- ^ converts new representation back to MChords
              -> CCEvalFunction
              -- a corresponding eval function, e.g. (overlapEval rootOnlyEq)
              -> (CCEval Double -> Double)
              -- what to evaluate. e.g. eMajMin
              -> (RefLab -> ChordLabel -> EqIgnore)
              -- ^ same but string
              -> NumData
              -- ^ Sampling frequency
              -> [[MChords]]
              -> [[MChords]]
              -> IO ([MChords], [MChords], [MChords], [MChords])
combineAllRand cfront cback feval ev eveq s garS garSGT = do
  pickedsources <- mapM (pickNU 6) garS      
    -- glass ceiling 
  ceilings  <- mapM (fusionBaseLine ev eveq feval) $ garSGT
  -- data fusion
  fusedAllR <- mapM (combineChordsM "FUSION" s cfront cback (listHandleGenericQuietD 5)) pickedsources
  -- majority vote
  mvAllR    <- mapM (combineChordsM "MVOTE"  s cfront cback (listMVGenericQuiet      4)) pickedsources 
  -- random picking
  rAllR     <- mapM (combineChordsM "RANDOM" s cfront cback (listRandomGenericQuiet  4))  pickedsources 
  return (ceilings, fusedAllR, mvAllR, rAllR)

evaluateFusionSong :: CCEvalFunction -> (CCEval Double -> Double) -> [MChords] -> IO (SongResults)
evaluateFusionSong ef ev mcs = do   
  let s   = songID . head $ mcs
  l <- mapM (evaluateSingle ef ev) mcs
  let ret = (s, l) 
  putStrLn . show $ (show s ++ show l)
  return (ret)

-- find fusion upper bound by comparing MIREX evaluations
fusionBaseLine :: (CCEval Double -> Double) -> (RefLab -> ChordLabel -> EqIgnore) -> CCEvalFunction -> [MChords] -> IO (MChords)
fusionBaseLine ev evv feval mc = do
    let tcls      = transpose . map (expandTimed . chords) . filter (\mc -> team mc /= "Ground-truth") $ mc
        gtmc      = head . filter (\mc -> team mc == "Ground-truth") $ mc
        tgt       = makeGT . expandTimed . chords $ gtmc
        newch     = zipWith (eqListTimed evv) tgt tcls
        --clist     = copyTimeStamps newch (chords gtmc)
        newmc     = MChords (collection gtmc) (year gtmc) ("CELIING") (songID gtmc) (newch) (Just (chords $ gtmc))
        frac      = ev . overlapRatioCCEval . evaluate feval $! newmc
        sid       = songID newmc 
    return (newmc)

-- find fusion upper bound by comparing MIREX evaluations
-- this should apply cfront and cback
--fusionBaseLineC :: ([ChordLabel] -> [a]) -> (a -> ChordLabel) -> (CCEval Double -> Double) -> (RefLab -> ChordLabel -> EqIgnore) -> CCEvalFunction -> [MChords] -> IO (MChords)
--fusionBaseLineC cfront cback ev evv feval mc = do
--    let tcls      = transpose . map (expandTimed . chords) . filter (\mc -> team mc /= "Ground-truth") $ mc
--        gtmc      = head . filter (\mc -> team mc == "Ground-truth") $ mc
--        tgt       = makeGT . expandTimed . chords $ gtmc
--        newch     = zipWith (eqListTimed evv) tgt tcls
--        --clist     = copyTimeStamps newch (chords gtmc)
--        newmc     = MChords (collection gtmc) (year gtmc) ("CELIING") (songID gtmc) (newch) (Just (chords $ gtmc))
--        frac      = ev . overlapRatioCCEval . evaluate feval $! newmc
--        sid       = songID newmc 
--    return (newmc)



copyTimeStamps :: [ChordLabel] -> [Timed ChordLabel] -> [Timed ChordLabel]
copyTimeStamps cl tcl = zipWith replaceCL cl tcl where
  replaceCL :: ChordLabel -> Timed ChordLabel -> Timed ChordLabel
  replaceCL cl tcl = Timed (cl) (getTimeStamps tcl)

-- rootOnlyEq
eqInList :: (RefLab -> ChordLabel -> EqIgnore) -> RefLab -> [ChordLabel] -> Bool
eqInList ef r cls = elem Equal . map (ef r) $ cls

-- This function is so ugly it makes me puke
eqListTimed :: (RefLab -> ChordLabel -> EqIgnore) -> (Timed RefLab) -> [Timed ChordLabel] -> (Timed ChordLabel)
eqListTimed ef r cls = h ef r cls where
  h ef r@(Timed rd rts) cls  | elem Equal . map (ef rd) $ (dropTimed cls) = Timed (refLab rd) rts
                             | otherwise                                  = Timed c rts where
                                c = snd . head . filter (\(eq, c) -> eq /= Equal) $ zip (map (ef rd) (dropTimed cls)) (dropTimed cls)

eqList :: (RefLab -> ChordLabel -> EqIgnore) -> RefLab -> [ChordLabel] -> ChordLabel
eqList ef r cls = h ef r cls where
  h ef r cls | elem Equal . map (ef r) $ cls = refLab r
             | otherwise                     = c where
              c = snd . head . filter (\(eq, c) -> eq /= Equal) $ zip (map (ef r) cls) cls

eqListB :: (RefLab -> ChordLabel -> EqIgnore) -> RefLab -> [ChordLabel] -> Bool
eqListB ef r cls = h ef r cls where
  h ef r cls | elem Equal . map (ef r) $ cls = True
             | otherwise                     = False

writePlotFile :: [MChords] -> IO ()
writePlotFile mcs = writeFile (toPlotFilename (head mcs)) (toPlotFile mcs)

toPlotFilename :: MChords -> String
toPlotFilename mc = fn where
  fn = "fusioncsv/"++c++y++s++"_aligned.csv"
  c = show . collection $ mc
  y = show . year $ mc 
  t = show . team $ mc 
  s = show . songID $ mc 

writeCSV :: FilePath -> [(SongID,[(Team,Double)])] -> IO ()
writeCSV fp rs = writeFile fp ss where
  ss      = intercalate "\n" $ header : (map lines rs)
  header :: String
  header  = intercalate "\t" $ (map (show . fst)) . snd . head $ rs
  lines :: (SongID,[(Team,Double)]) -> String
  lines r = intercalate "\t" $ ((show.fst) r) : (map (show.snd) $ snd r)

toPlotFile :: [MChords] -> String
toPlotFile mcs = intercalate "\n" $ (["no\t"] ++ (map (fbracket . show . line) mcs)) where
  line :: MChords -> [String]
  line mc = (map show) . dropTimed . chords $ mc

fbracket :: String -> String
fbracket = filter (/= '[') . filter (/= ']')

evaluateSingle :: CCEvalFunction -> (CCEval Double -> Double) -> MChords -> IO (Team,Double)
evaluateSingle ef ev mc = do
  let t = team mc
      d = ev . overlapRatioCCEval . evaluate ef $! mc
  return (d `seq` (t,d))

sampletoChordClass :: NumData -> [MChords] -> [[ChordClass]]
sampletoChordClass spl = ((map.map) toChordClass) . (sampleMChords spl)

round2D :: (Fractional a, RealFrac r) => r -> a
round2D d = (fromInteger $ round $ d * (10^2)) / (10.0^^2)

-- combine the chords in an [MChords] using a Strategy
combineChordsM :: (Show a, Ord a) => 
                  Team ->
                  NumData -> 
                  ([ChordLabel] -> [a]) ->
                  (a -> ChordLabel) -> 
                  ([a] -> [[a]] -> IO ([a])) ->
                  [MChords] -> 
                  IO (MChords)
combineChordsM t spl cfront cback strategy mc = do
  let gt = filter (\m -> (team m) == "Ground-truth") mc 
  let others = filter (\m -> (team m) /= "Ground-truth") mc 

  -- convert from ChordLabel with cfront:
  let newrep = map (cfront . dropTimed . chords) $ mc
  let newrepgt = map (cfront . dropTimed . chords) $ gt
  -- fuse the converted chords
      dom = nub . cfront $ allMMChords      
  --putStrLn . show $ dom
  fusedr <- strategy dom newrep
  --putStrLn $ "newrep :"
  --putStrLn . show $ newrep
  --writeFile "chord.csv" (show newrep)
  --putStrLn $ "gt:"
  --putStrLn . show $ newrepgt
  --writeFile "fusion.csv" (show fusedr)
  --putStrLn $ "df out length:"
  --putStrLn . show $ fusedr
  -- convert back to [Chordlabel]:
  let fusedrTC = map cback fusedr
  -- reattach the timestamps 
  let fusedHT = attachTime spl fusedrTC
  -- make new MChords:
  let newmc = insertNewChords (mc!!0) fusedHT t
  return (newrep `seq` fusedr `seq` fusedrTC `seq` fusedHT `seq` newmc)

insertNewChords :: MChords -> [Timed ChordLabel] -> String -> MChords
insertNewChords (MChords c y t s ch g) newch name = (MChords c y name s newch g)

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

evaluateL :: ([Timed RefLab] -> [[Timed ChordLabel]] -> a) -> [MChords] -> a
evaluateL ef mc = case groundTruth . head $ mc of
  (Just gt) -> ef (makeGT gt) (map chords mc)
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
