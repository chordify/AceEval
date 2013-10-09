module AceMIREX  ( Collection (..)
                 , Year (..)
                 , Format (..)
                 , Team
                 , MChords (..)
                 , toFileName
                 , fromFileName
                 , toYear
                 , toCollection
                 , errorise
                 ) where
                 
import HarmTrace.Base.Time     ( Timed )
import HarmTrace.Base.Chord    ( ChordLabel )
import Data.List               ( intercalate )
import Data.Char               ( isDigit )
import Text.Printf             ( printf )
import System.FilePath         ( (</>), (<.>), splitDirectories
                               , takeExtension, dropExtension )

data Format     = JS | LAB deriving (Show, Eq)
data Collection = Billboard | Beatles   deriving (Show, Eq)
data Year       = Y2010 | Y2011 | Y2012
type Team       = String

instance Show Year where
  show Y2010 = "2010"
  show Y2011 = "2011"
  show Y2012 = "2012"
  
data MChords    = MChords { collection  :: Collection
                          , year        :: Year
                          , team        :: String
                          , songID      :: Int
                          , chords      :: [Timed ChordLabel]
                          , groundTruth :: Maybe [Timed ChordLabel]
                          }  
                          
instance Show MChords where
  show (MChords c y t i _cs _mgt) = intercalate " " [show c, show y, t, show i]

fromFileName :: FilePath -> (Year, Collection, Team, Int, Format)
fromFileName fp = case reverse . splitDirectories $ fp of
        (fn : tm : c : y : base) -> ( errorise (toYear y)
                                    , errorise (toCollection c)
                                    , tm
                                    , getId fn
                                    , errorise . toFormat. takeExtension $ fn)
        _ -> error ("fromFileName: invalid AceEval filepath" ++
                    " (Year / Collection / team / file . extension)")

-- Parses: chordmrx09000008.js, chords1234.js, audio1234.lab, 3456.lab
getId :: String -> Int
getId s = read . dropWhile (not . isDigit) . dropExtension $ s
                    
toCollection :: String -> (Maybe Collection, String)
toCollection s = case s of
                  "bb"        -> (Just Billboard, [])
                  "bs"        -> (Just Beatles, [])
                  "billboard" -> (Just Billboard, [])
                  "beatles"   -> (Just Beatles, [])
                  "Billboard" -> (Just Billboard, [])
                  "Beatles"   -> (Just Beatles, [])
                  m           -> (Nothing, "unrecognised collection: " ++ m)
                    
toYear :: String -> (Maybe Year, String)
toYear s = case s of
            "2010" -> (Just Y2010, [])
            "2011" -> (Just Y2011, [])
            "2012" -> (Just Y2012, [])
            m      -> (Nothing, "unrecognised year: " ++ m)
                    
toFormat :: String -> (Maybe Format, String)
toFormat s = case s of
              ".js"  -> (Just JS , [])
              ".lab" -> (Just LAB, [])
              _      -> (Nothing , "unrecognised extension: " ++ s )

errorise :: (Maybe a, String) -> a
errorise (Just a,  _) = a
errorise (Nothing, e) = error e 
                    
toFileName :: FilePath -> Year -> Collection -> Team -> Int -> FilePath
toFileName dir y c t i = dir </> show y </> show c </> t </> toID where

  toID :: String
  toID = case c of
           Beatles   -> printf "chordmrx09000%03d.js" i
           Billboard -> show i <.> "js"