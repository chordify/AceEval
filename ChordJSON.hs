{-# OPTIONS_GHC -Wall          #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types #-}
module ChordJSON                     ( Collection (..)
                                     , Year (..)
                                     , Team
                                     , MChords (..)
                                     , CData (..)
                                     , evaluateMChords
                                     , evaluateMChordsVerb
                                     , evaluateMirex
                                     )where

import HarmTrace.Base.Time           ( Timed (..), BeatTime (..)
                                     , offset, onset, duration, pprint )
import HarmTrace.Base.Chord          ( ChordLabel, Chord (..) )
-- import HarmTrace.Base.ChordTokenizer -- ( pChord )
import HarmTrace.Base.Parse        -- ( parseDataWithErrors )

import Data.Foldable                 ( foldrM )
import Data.List                     ( intercalate, genericLength, partition )
import Data.Maybe                    ( isJust, fromJust )
import System.Directory              ( getDirectoryContents )
import System.FilePath               ( (</>) )
import System.IO                     ( hPutStrLn, stderr )
import Control.Concurrent.ParallelIO.Global ( parallel )

import Evaluation 
--------------------------------------------------------------------------------
-- Parsing ChordJSON into HarmTrace chords
--------------------------------------------------------------------------------

data Collection = Billboard | Beatles   deriving (Show, Eq)
data Year       = Y2010 | Y2011 | Y2012

data MChords    = MChords { collection :: Collection
                          , year       :: Year
                          , team       :: String
                          , songID     :: Int
                          , cdata      :: [CData]
                          }  
                          
data CData      = CData   { desc       :: String
                          , chords     :: [Timed ChordLabel]
                          }

type Team       = String

instance Show Year where
  show Y2010 = "2010"
  show Y2011 = "2011"
  show Y2012 = "2012"
                          
instance Show MChords where
  show (MChords c y t i _cs) = intercalate " " [show c, show y, t, show i]

--------------------------------------------------------------------------------
-- MIREX data parsing
--------------------------------------------------------------------------------

-- | Parses a MIREX results file
pMIREXjs :: Year -> Collection -> Parser MChords
pMIREXjs y c'' = f <$> pHeader <*> pJSList pChordList <*> pFooter
  where f (s, c, i) cs (s', c', i', d) 
          | s == s' && c == c' && i == i' && c == c''
                      = MChords c y s i (zipWith CData d cs)
          | otherwise = error "pMIREXjs: conflicting meta data"

-- A header in the form: "var some_name = "
pHeader :: Parser (String, Collection, Int)
pHeader = pVar "data" <?> "header"

-- A footer containing "; \n var another_name = ["some","descriptions"]; 
pFooter :: Parser (String, Collection, Int, [String])
pFooter = g <$> (lexeme (pSym ';') *> pVar "seriesNames") 
            <*>  pJSList pQuotedString
            <*   lexeme (pSym ';')
            <?>  "footer"
            where g (s, c, i) d = (s, c, i, d)

-- | a var statement: "var pmp3chordschordmrx09000087_sfx ="
pVar :: String -> Parser (String, Collection, Int)
-- pVar sfx = f <$> (pString "var "   *> pList1_ng pAscii )
pVar sfx = f <$> (pString "var "   *> pTeam )
             <*> (pString "chords" *> pMaybe (pString "chordmrx"))
             <*>  pInteger <* pSym '_' <* pString sfx <* (lexeme $ pString " =")
             <?> "var statement"
             where f tm mb s = (tm, maybe Billboard (const Beatles) mb, s)

pTeam :: Parser String
pTeam = snoc <$> pBetween 1 4 pLetter <*> pDigit  <?> "team" where
  snoc l e = l ++ [e]

-- | parsers a single chords list
pChordList :: Parser [Timed ChordLabel]
pChordList = pJSList . pBraces $ pTimedChord

-- {o: 187.13, f: 190.522, l: "D:(9)", a: 0},
pTimedChord :: Parser (Timed ChordLabel)
pTimedChord = timedData <$> (pString "o: "   *> pDoubleRaw)
                        <*> (pString ", f: " *> pDoubleRaw)
                        <*> (pString ", l: \"" *> pChord   )
                        <*   pString "\", a: " <* (pSym '0' <|> pSym '1') 
                        <?> "Onset, offset and chord" where
                        
                        timedData on off c = Timed c [Time on, Time off]

-- replace by listparser?
-- Parsers a typical javascript list: [elem, elem, etc.]
pJSList :: ParserTrafo a [a]
pJSList p = pBrackets . pList1Sep_ng pComma $ p

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
     putStrLn . (show mc ++ ) . show . evaluate (\a b -> pp $ ef a b) $ mc
  

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
              -> ([a] -> Double)
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
            do r <- getTeamFiles tm 
                    >>= parallel . map evaluateMChord >>= return . af
               putStrLn $ "Team " ++ tm ++ " average: " ++ show r
               return r

          -- | returns the files for one team
          getTeamFiles :: Team -> IO [(Team, FilePath)]
          getTeamFiles tm = getCurDirectoryContents (baseDir </> tm)
                            >>= return . map (\f -> (tm, baseDir </> tm </> f))

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
  do txt <- readFile fp 
     case parseDataWithErrors (pMIREXjs y c) txt of
       (mc, []) -> fillHoles $ mc
       (_ , er) -> error ("parsing file "  ++ fp ++ " yields the following " ++
                       "parse errors:\n" ++ concatMap (\e -> show e ++ "\n") er)
                       
fillHoles :: MChords -> IO MChords
fillHoles mc = do f <- mapM fill . cdata $ mc
                  return mc { cdata = f } where
  
  fill :: CData -> IO CData
  -- NB filterZeroLen should go somewhere else
  fill cd = do cs <- foldrM step [] (chords cd) >>= filterZeroLen 
               return cd {chords = cs} where
  
    step :: Timed ChordLabel ->[Timed ChordLabel] 
         -> IO [Timed ChordLabel]
    step a []     = return [a]
    step a (b:ts) 
      | off == on = return (a        : b : ts)
      | otherwise = warn >> return (a : hole : b : ts)
                       
           where off  = offset a
                 on   = onset  b 
                 hole = Timed UndefChord [Time off, Time on]
                 warn = putErrStrLn ("Warning: found a hole in " ++ show mc ++" "
                             ++ desc cd ++ ": " ++ show off ++ " - " ++ show on)

filterZeroLen :: Show a => [Timed a] -> IO [Timed a] 
filterZeroLen td = do let (zero, good) = partition (\x -> duration x == 0) td
                      mapM_ warn zero >> return good where
                   
  warn :: Show a => Timed a -> IO ()
  warn t = putErrStrLn ("Warning: found zero length segment: " ++ pprint t )
                       
-- | Applies an evaluation function to an 'MChords' 
evaluate :: ([Timed RefLab] -> [Timed ChordLabel] -> a) 
         -> MChords -> a
evaluate ef mc = case cdata mc of
  [CData "Ground-truth" gt, CData "Prediction" p] -> ef (makeGT gt) p -- $! p
                                                     
  _  -> error "evaluate: I did not find a ground-truth and chord prediction"

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
