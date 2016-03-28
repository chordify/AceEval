module ACE.MIREX.Data  ( Collection (..)
                       , Year (..)
                       , Format (..)
                       , Team
                       , MChords (..)
                       -- , getFormat
                       , fromFileName
                       , toFileName
                       -- , toLabGT
                       , toYear
                       , toCollection
                       , toFormat
                       , errorise
                       -- , mChordStats
                       ) where

import HarmTrace.Base.Time     ( Timed )
import HarmTrace.Base.Chord    ( ChordLabel )
import ACE.Evaluation.ChordEq  ( RefLab )
import Data.Char               ( toLower )
import Text.Printf             ( printf )
import System.FilePath         ( (</>), splitDirectories, joinPath
                               , takeExtension, dropExtensions )

data Format     = JS | LAB deriving (Show, Eq)
data Collection = Billboard2012 | Billboard2013 | Beatles
                | Unkown  deriving (Show, Eq)
data Year       = Y2010 | Y2011 | Y2012 | Y2013 | Other
type Team       = String

instance Show Year where
  show Y2010 = "2010"
  show Y2011 = "2011"
  show Y2012 = "2012"
  show Y2013 = "2013"
  show Other = "Other"

data MChords    = MChords { collection  :: Collection
                          , year        :: Year
                          , team        :: String
                          , songID      :: Int
                          , chords      :: [Timed ChordLabel]
                          , groundTruth :: Maybe [Timed RefLab]
                          }

instance Show MChords where
  show (MChords c y t i _cs _mgt) = unwords [show c, show y, t, show i]

fromFileName :: FilePath -> (FilePath, Year, Collection, Team, Int, Format)
fromFileName fp = case reverse . splitDirectories $ fp of
        (fn : tm : c : y : base) -> ( joinPath (reverse base)
                                    , errorise (toYear y)
                                    , errorise (toCollection c)
                                    , tm
                                    , getId fn
                                    , errorise . toFormat. takeExtension $ fn)
        _ -> error ("fromFileName: invalid AceEval filepath" ++
                    " (Year / Collection / team / file . extension)")

-- Parses: chordmrx09000008.js, chords1234.js, audio1234.lab, 3456.lab
getId :: String -> Int
-- getId s = read . dropWhile (not . isDigit) . dropExtension $ s
getId s = read . reverse . take 4 . reverse . dropExtensions $ s

toCollection :: String -> (Maybe Collection, String)
toCollection s = case map toLower s of
                  "bb12"          -> (Just Billboard2012, [])
                  "bb13"          -> (Just Billboard2013, [])
                  "bs"            -> (Just Beatles, [])
                  "billboard2012" -> (Just Billboard2012, [])
                  "billboard2013" -> (Just Billboard2013, [])
                  "beatles"       -> (Just Beatles, [])
                  m               -> (Nothing, "unrecognised collection: " ++ m)

toYear :: String -> (Maybe Year, String)
toYear s = case s of
            "2010" -> (Just Y2010, [])
            "2011" -> (Just Y2011, [])
            "2012" -> (Just Y2012, [])
            "2013" -> (Just Y2013, [])
            m      -> (Nothing, "unrecognised year: " ++ m)

toFormat :: String -> (Maybe Format, String)
toFormat s = case s of
              ".js"  -> (Just JS , [])
              ".lab" -> (Just LAB, [])
              ".txt" -> (Just LAB, [])
              _      -> (Nothing , "unrecognised extension: " ++ s )

errorise :: (Maybe a, String) -> a
errorise (Just a,  _) = a
errorise (Nothing, e) = error e


toFileName :: FilePath -> Year -> Collection -> Team -> Int -> Format
           -> FilePath
toFileName dir y c t i f = dir </> show y </> show c </> t </> toID where

  toID :: String
  toID = case (f,c) of
           (JS,  Beatles)       -> printf "chordmrx09000%03d.js" i
           (LAB, Beatles)       -> printf "chordschordmrx09000%03d.lab" i
           (JS,  Billboard2012) -> printf "%04d.js" i
           (JS,  Billboard2013) -> printf "%04d.js" i
           (LAB, _            ) -> printf "%04d.lab" i
           _                    -> error "toID: filetype or collection mismatch"
